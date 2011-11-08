/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package event

      
      // def where[T: Manifest](pred: T => Boolean): Hook
      // def having[T: Manifest](value: T): Hook
      // def filtered(hook: Hook, filts: Filter*): Hook

// def gather[T: Manifest](f: T => Boolean): Gatherer[T] = {
//   val gatherer = new Gatherer[T](f)
//   addHook(gatherer.hook)
//   gatherer
// }


import symtab.{ Flags, SymbolTable }
import scala.collection.{ mutable, immutable, JavaConversions }
import scala.concurrent.DelayedLazyVal
import mutable.ListBuffer
import java.util.concurrent.{ LinkedBlockingQueue }
import scala.reflect.internal.event.{ EventsUniverse, Events }

trait EventsSymbolTable extends EventsUniverse with Events with EventStrings with Processing {
  outer: SymbolTable =>
  
  /*val EV: EventModel {
    val global: outer.type
  }*/
  
  abstract class EventModel extends super.EventModel with Strings with ProcessingUtil {
    model =>

    val global: SymbolTable

    type EventResponse = Unit
    val NoResponse     = ()

    type Phase  = nsc.Phase
    val NoPhase = nsc.NoPhase
    def currentPhase: Phase = global.phase
    override def symbolPos(sym: Symbol): Position = sym.pos
    override def treePos(tree: Tree): Position = tree.pos
    implicit lazy val phaseOrdering: Ordering[Phase] = Ordering[Int] on (_.id)

    protected[event] def fatal(msg: String): Nothing = Predef.error(msg)
    
    override protected def involvedNames(entity: Any): List[Name] = entity match {
      case x: Scope       => x.toList flatMap involvedNames // todo: add membership check interface
      case x: BaseTypeSeq => x.toList flatMap involvedNames
      case _              => super.involvedNames(entity)
    }
    override protected def involves(entity: Any, name: Name): Boolean = entity match {
      case x: Scope       => x.lookup(name) != NoSymbol
      case _              => super.involves(entity, name)
    }

    protected def isDebug                = global.settings.debug.value
    protected def log(msg: String): Unit = global.log(msg)
    protected def dlog(msg: => String): Unit = if (isDebug) log(msg)

    object NoEvent extends Event {
      def tag = "NoEvent"
      protected def participants = Nil
    }

    /** Active event hooks.
     */
    private var _eventHooks: List[Hook] = Nil

    def eventHooks          = _eventHooks
    def removeHook(h: Hook) = _eventHooks = _eventHooks filterNot (_ eq h)
    def addHook(h: Hook)    = {
      if (_eventHooks contains h) ()
      else {
        log("Adding hook for " + h)
        _eventHooks = _eventHooks :+ h
      }
    }
    def clearHooks()        = _eventHooks = Nil
    // 
    // def firehose = FireHose.on
    // def dip(capacity: Int) = new Hose(capacity) on()
    // 
    // class Hose(val capacity: Int) extends immutable.Iterable[Event] {
    //   protected val hose = new LinkedBlockingQueue[Event](capacity)
    //   protected val hook: Hook = Hook({ ev: Event =>
    //     hose add ev
    //     if (shouldStop())
    //       off()
    //   })
    //   private var filters: List[Filter] = Nil
    //   
    //   def shouldStop()     = hose.size >= capacity
    //   def on(): this.type  = { addHook(hook) ; this }
    //   def off(): this.type = { removeHook(hook) ; this }
    //   def drain()          = hose.clear()      
    //   def iterator         = JavaConversions.asScalaIterator(hose.iterator())
    // }
    // object FireHose extends Hose(Int.MaxValue) {
    //   
    // }
    
    case class NewParse(tree: Tree) extends TreeEvent {
      def tag = "newParse"
    }
    case class ThisPhaseDone(override val unit: CompilationUnit) extends Event {
      def tag = "phaseDone"
      protected def participants: List[Any] = List(unit)
    }
    case class NewScope(scope: Scope) extends Event {
      def tag = "newScope"
      protected def participants: List[Any] = List(scope)
    }
    case class NewBaseTypeSeq(bts: BaseTypeSeq) extends Event {
      def tag = "newBaseTypeSeq"
      protected def participants: List[Any] = List(bts)
    }
    
    trait Hook extends AbsHook {
      def start(): this.type = {
        addHook(this)
        this
      }
      def stop(): this.type = {
        removeHook(this)
        this
      }
    }
    object Hook extends HookCompanion {      
      class SimpleHook(f: Event => Unit) extends Hook {
        def action(ev: Event): Unit = f(ev)
      }
      class LoggerHook(val fmt: String) extends Hook {
        override def show(ev: Event) = Console println (ev formattedString fmt)
        def action(ev: Event): Unit = show(ev)
      }
      def apply(pf: Event =>? Unit): Hook = {
        val (filt, fn) = Filter decompose pf
        
        new SimpleHook(fn) filterBy filt
      }
      def start(pf: Event =>? Unit): Hook = apply(pf).start
      def log(): Hook = log("[%ph] %ev %po")
      def log(fmt: String): Hook = new LoggerHook(fmt)
    }
    
    object EVDSL {
      object tpe {
        def <:<(toMatch: Type): Filter = 
          Filter("<:<", _.types exists (_ <:< toMatch))
      }

      object ph {
        import phaseOrdering._

        def <=(id: Int): Filter = <=(phaseWithId(id))
        def >=(id: Int): Filter = >=(phaseWithId(id))

        def <=(maxPhase: Phase): Filter    = Filter("phase < " + maxPhase.name, _.phase <= maxPhase)
        def >=(minPhase: Phase): Filter    = Filter(minPhase.name + " < phase", minPhase <= _.phase)
      }

      object ev {
        def +(mask: Long) = Filter(
          "sets " + flagsString(mask), 
          _ match { case x: FlagEvent[_] => x didAddFlag mask ; case _ => false }
        )
        def -(mask: Long) = Filter(
          "clears " + flagsString(mask), 
          _ match { case x: FlagEvent[_] => x didLoseFlag mask ; case _ => false }
        )
        def +/-(mask: Long) = Filter(
          "changes " + flagsString(mask), 
          _ match { case x: FlagEvent[_] => x didChangeFlag mask ; case _ => false }
        )
      }
    }
    
    
    trait Filter extends AbsFilter {      
    }
    object Filter extends FilterCompanion {
      /** Look ma, it's short circuiting. */
      type BoolMerge  = Boolean => (=> Boolean) => Boolean
      case class Comb(name: String, op: BoolMerge) { }
      
      val AndComb    = Comb("and", x => y => x && y)
      val OrComb     = Comb("or", x => y => x || y)

      def combine(f1: Filter, f2: Filter, comb: Comb): Filter = {
        val tag           = joinString(f1.tag, comb.name, f2.tag)
        
        apply(tag, (x: Event) => comb.op(f1(x))(f2(x)))
      }
      class SimpleFilter(override val tag: String, f: Event => Boolean) extends Filter {
        def apply(ev: Event) = f(ev)
      }

      def and(filters: Filter*) = if (filters.isEmpty) empty else filters.reduceLeft(combine(_, _, AndComb))
      def or(filters: Filter*)  = if (filters.isEmpty) empty else filters.reduceLeft(combine(_, _, OrComb))      
      def not(filter: Filter)   = apply("!" + filter.tag, ev => !filter(ev))
      def apply(tag: String, f: Event => Boolean): Filter = new SimpleFilter(tag, f)
      def apply(f: Event => Boolean): Filter = apply("", f)
    }
    
    def <<(ev: Event): Unit = {
      if (eventsOn) {
        dlog(ev.toString)
        eventHooks foreach (_ applyIfDefined ev)
      }
    }
  }
}
