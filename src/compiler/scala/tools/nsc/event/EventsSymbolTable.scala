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

trait EventsSymbolTable extends EventsUniverse
                        with EventStrings
                        with Processing
                        with symtab.Positions
                        with GeneralEvents {
  outer: SymbolTable =>
  
  abstract class EventModel extends super.EventModel
                            with Strings
                            with ProcessingUtil
                            with ControlEvents {
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
    

    trait Hook extends AbsHook {
      def start(): this.type = {
        addHook(this)
        this
      }
      def stop(): this.type = {
        removeHook(this)
        this
      }
      
      def startBlock(): Unit = ()
      def endBlock(): Unit = ()
    }
    object Hook extends HookCompanion {
      class SimpleHook(f: Event => EventResponse) extends Hook {
        def action(ev: Event): EventResponse = f(ev)
      }
      class LoggerHook(val fmt: String) extends Hook {
        override def show(ev: Event) = Console println (ev formattedString fmt)
        def action(ev: Event): EventResponse = show(ev)
      }
      class IndentationHook(f: (Int) => (Event) => EventResponse) extends Hook {
        private var _indent: Int = 0
        def indentSize = _indent
        override def startBlock() {
          _indent += 1 
        }
  
        override def endBlock() {
          //Cyclic errors can still break this assert. We should have a fallback which closes blocks automatically
          // by looking into exceptions stacktrace
          
          assert(_indent >= 0, "indent counter below zero")
          _indent -= 1
        }

        // Used when exception is propagated and 
        // we revert to last known place
        def resetIndentation(a: Int) = _indent = a
        override def action(ev: Event): EventResponse = {
          f(_indent)(ev)
        }
      }
      
      def indentation(pf: Int => Event =>? EventResponse): IndentationHook = {
        val (filt, fn) = Filter decomposeIndent pf
        new IndentationHook(fn) filterBy filt
      }
      
      def apply(pf: Event =>? EventResponse): Hook = {
        val (filt, fn) = Filter decompose pf
        new SimpleHook(fn) filterBy filt
      }
      def start(pf: Event =>? EventResponse): Hook = apply(pf).start
      def log(): Hook = log("[%ph] %ev %po")
      def log(fmt: String): Hook = new LoggerHook(fmt)
    }
    
    trait IndendationFilterCompanion {
      self: FilterCompanion =>
      
      type IndentSign[R] = Int => Event => R
      
      def decomposeIndent[R](f: IndentSign[R]): (Filter, IndentSign[R]) = {
        val (filtr, _) = decompose(f(0))
        (filtr, f)
      }
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
        def ==(id: Int): Filter = ==(phaseWithId(id))

        // phaseWithId might be called before the phases are yet initialised
        // so call them properly when compiler reaches already the phase
        def <=(maxPhase: => Phase): Filter = Filter("phase < " + maxPhase.name, _.phase <= maxPhase)
        def >=(minPhase: => Phase): Filter = Filter(minPhase.name + " < phase", minPhase <= _.phase)
        def ==(eqPhase : => Phase): Filter = Filter("phase eq " + eqPhase.name, eqPhase == _.phase)
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
      
      object id {
        def <=(id: Int): Filter = Filter("event.id < " + id, _.id <= id)   
        def >=(id: Int): Filter = Filter(id + " < event.id", id <= _.id)
      }
    }
    
    
    trait Filter extends AbsFilter {      
    }
    object Filter extends FilterCompanion with IndendationFilterCompanion {
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
    
    def <<(ev: Event): EventResponse = {
      if (eventsOn) {
        dlog(ev.toString)
        //assert(!ev.isInstanceOf[DoneBlock])
        eventHooks foreach (_ applyIfDefined ev)
      }
    }
    
    def <<<(ev: Event): EventResponse = {
      if (eventsOn) {
        // start block
        ev.blockStart = true
        dlog(ev.toString)
        //assert(!ev.isInstanceOf[DoneBlock])
        eventHooks foreach (h => {h applyIfDefined ev; h.startBlock })
      }
    }
    
    def >>>(ev: Event): EventResponse = {
      if (eventsOn) {
        // end block
        dlog(ev.toString)
        //assert(ev.isInstanceOf[DoneBlock], ev.getClass + " should be an instance of DoneBlock")
        // Should update tag to super.tag`-done`
        // but cannot do it easily without forwarding everything
        eventHooks foreach (h => {h applyIfDefined ev; h.endBlock})
      }
    }
    
    def >>(ev: Event): EventResponse = {
      if (eventsOn) {
        // end block
        dlog(ev.toString)
        //assert(ev.isInstanceOf[DoneBlock], ev.getClass + " should be an instance of DoneBlock")
        // Should update tag to super.tag`-done`
        // but cannot do it easily without forwarding everything
        eventHooks foreach (h => {h applyIfDefined ev; h.endBlock})
      }
    }    
  }
}
