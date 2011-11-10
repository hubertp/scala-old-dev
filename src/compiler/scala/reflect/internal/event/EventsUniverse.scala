package scala.reflect
package internal
package event

import annotation.elidable

trait EventsUniverse extends AnyRef with Events {
  outer: SymbolTable =>
  
  val EV: EventModel {
    val global: outer.type
  }
  
  abstract class EventModel extends AllEvents {
    val global: SymbolTable
    var isInitialized = false
    private var eventIds = 0
    
    type EventResponse
    val NoResponse: EventResponse
    
    type Filter <: AbsFilter
    val Filter: FilterCompanion

    type Hook <: AbsHook
    val Hook: HookCompanion

    type Phase <: {
      def name: String
    }
    val NoPhase: Phase
    def currentPhase: Phase
    
    type CompilationUnit <: {
      def source: Any
    }
    def currentUnit: CompilationUnit
    def currentPos: Position

    type =>?[-T, +R]  = PartialFunction[T, R]
    type JClass[T]    = java.lang.Class[T]
    type HookFn[T]    = PartialFunction[Event, T]
    
    val TypeManifest: Manifest[Type]
    val SymbolManifest: Manifest[Symbol]
    val TreeManifest: Manifest[Tree]
    val NameManifest: Manifest[Name]
    val PositionManifest: Manifest[Position]
    
    /** Strings.  Have to be careful because creating Strings can generate a
     *  great many events itself.
     */
    def eventsOn: Boolean
    def anyString(x: Any): String
    def flagsString(flags: Long): String
    def posString(pos: Position): String
    def formatTypeString(tpe: Type): String

    // def eventFormat: String = "%foo %bar" // todo
    def joinString(xs: Any*) = xs map anyString filterNot (_ == "") mkString " "
    def symbolPos(sym: Symbol): Position = NoPosition
    def treePos(tree: Tree): Position = NoPosition
    
    protected def cast[T](x: Any): T = x.asInstanceOf[T]
    protected def anyGetClass(x: Any): JClass[_] = cast[AnyRef](x).getClass
    protected def involvedNames(entity: Any): List[Name] = entity match {
      case x: Type            => involvedNames(x.typeSymbol)
      case x: Symbol          => involvedNames(x.name)
      case x: Tree            => nameInTree(x).toList ++ involvedNames(x.symbol)
      case xs: Traversable[_] => xs flatMap involvedNames toList
      case x: Name            => List(x)
      case _                  => Nil
    }

    /** Ignoring the symbol here. */
    def nameInTree(tree: Tree): Option[Name] = tree match {
      case x: DefTree   => Some(x.name)
      case x: RefTree   => Some(x.name)
      case _ =>
        tree match {
          case ExistentialTypeTree(tpt, _)  => nameInTree(tpt)
          case AppliedTypeTree(tpt, _)      => nameInTree(tpt)
          case _                            => None
        }
    }    
    protected def involves(entity: Any, name: Name): Boolean =
      involvedNames(entity) exists (x => isSameName(x, name))

    /** Depending on our confidence in TypeNames and TermNames doing
     *  the right thing, can be adjusted.
     */
    def isSameName(n1: Name, n2: Name) = n1.toString == n2.toString
    
    def resetEventsCounter {
      eventIds = 0
    }

    /** The base class for all Events.
     */
    abstract class Event {
      def tag: String   // A short description of this event
      def lName: String = tag // Long description if different
      protected def participants: List[Any]
      var id = { eventIds += 1; eventIds }
      var blockStart = false // TODO refactor to VisEvent

      // record the phase and unit at creation
      val phase: Phase          = currentPhase
      val unit: CompilationUnit = currentUnit
      
       
      // Sometimes it is necessary to force the actual string representation
      // at the creation time since we often deal with refs and not primitive vals
      protected val eventStringRep: String = ""

      protected def defaultPos = currentPos
      private var _pos: Position = defaultPos
      def pos: Position         = _pos
      def withPos(pos: Position): this.type = { _pos = pos ; this }
      def unitString =
        if (unit == null) "<unknown>"
        else unit.source.toString
  
      def eventPosString: String = posString(pos) match {
        case ""   => unitString
        case x    => x
      }
      
      def names: List[Name]        = participants flatMap involvedNames distinct
      def symbols: List[Symbol]    = participants collect { case x: Symbol if x != NoSymbol => x }
      def types: List[Type]        = participants collect { case x: Type if x != NoType => x }
      def trees: List[Tree]        = participants collect { case x: Tree if !x.isEmpty => x }
      def classes: List[JClass[_]] = participants map anyGetClass distinct
      
      def matches[T: Manifest](p: T => Boolean) = matching[T](p).nonEmpty
      def matching[T: Manifest](p: T => Boolean): List[T] = {
        implicit def castPredicate[U](p: T => Boolean): U => Boolean =
          (x: U) => p(cast[T](x))

        val m = manifest[T]
        val res = 
          if (m == SymbolManifest) symbols filter p
          else if (m == TypeManifest) types filter p
          else if (m == TreeManifest) trees filter p
          else if (m == PositionManifest) List(pos) filter p
          else if (m == NameManifest) names filter p
          else {
            val clazz = manifest[T].erasure
            val xs = participants collect { case x: AnyRef if clazz isAssignableFrom x.getClass => cast[T](x) }
            xs filter p
          }
        res map cast[T]
      }
      val formatterMap: Map[String, () => String] = Map(
        "ev" -> (() => eventString),
        "tg" -> (() => tag),
        "cu" -> (() => unitString),
        "ph" -> (() => phase.name),
        "na" -> (() => names mkString ", "),
        "po" -> (() => eventPosString),
        "id" -> (() => "#" + id),
        "ln" -> (() => lName)
      )
      def formattedString(format: String): String = {
        val sb = new StringBuilder
        def loop(fmt: String): String = {
          if (fmt.length < 3) {
            sb append fmt
            sb.toString
          }
          else if (fmt.head != '%') {
            sb append fmt.head
            loop(fmt.tail)
          }
          else if (fmt startsWith "%%") {
            sb append '%'
            loop(fmt drop 2)
          }
          else formatterMap.get(fmt.tail take 2) match {
            case Some(f)  => sb append f() ; loop(fmt drop 3)
            case _        => sb append fmt.head ; loop(fmt.tail)
          }
        }
        loop(format)
      }

      def involvesClass(clazz: JClass[_]) = classes exists (clazz isAssignableFrom _)
      def involvesName(name: Name)        = names exists (x => isSameName(x, name))
      
      def hasStringWhich(f: String => Boolean) = participants.iterator map anyString exists f
      
      def eventString = joinString(participants)
      override def toString = eventString
    }
    trait UnaryEvent[+T] extends Event {
      def value: T
      override def eventString = joinString(value)
      protected def participants: List[Any] = List(value)
    }
    abstract class BinaryEvent[+T] extends Event {
      def value1: T
      def value2: T
      def binaryOp: Any
      
      override def eventString              = joinString(value1, binaryOp, value2)
      protected def participants: List[Any] = List(value1, value2)
    }
    trait SymEvent extends UnaryEvent[Symbol] {
      def sym: Symbol
      def value = sym
      
      override protected val eventStringRep       = anyString(value)
      override protected def defaultPos = symbolPos(sym)
    }
    trait TwoSymEvent extends BinaryEvent[Symbol] { 
      override protected def defaultPos = symbolPos(value1)
    }
    trait TypeEvent extends UnaryEvent[Type] {
      def tpe: Type
      def value = tpe
      def sym: Symbol = tpe.typeSymbol
      override protected def defaultPos = symbolPos(sym)
    }
    trait TwoTypeEvent extends BinaryEvent[Type] { 
      override protected def defaultPos = symbolPos(value1.typeSymbol)
    }
    trait TreeEvent extends UnaryEvent[Tree] {
      def tree: Tree
      def value = tree
      override protected def defaultPos = treePos(tree)
      override protected val eventStringRep = joinString(value)
      override def eventString = eventStringRep
    }
    
    trait TwoTreeEvent extends BinaryEvent[Tree] { 
      override protected def defaultPos = treePos(value1)
    }
    
    trait TreeTypeEventUnary extends TypeEvent {
      def tree: Tree
      override protected def participants: List[Any] = List(tree, tpe)
      override protected val eventStringRep = joinString(tree, tpe)
      override def eventString = eventStringRep
      override protected def defaultPos = treePos(tree)
    }
    
    trait TreeTypeEventBinary extends TwoTypeEvent {
      def tree1: Tree
      def tree2: Tree
      override protected def participants: List[Any] = List(tree1, value1, tree2, value2)
      override def eventString = joinString(tree1, value1, tree2, value2)
      override protected def defaultPos = treePos(tree1)
    }

    trait FlagEvent[+T] extends Event {
      def carrier: T
      def oldFlags: Long
      def newFlags: Long
      def mask: Long
      
      def addedFlags             = newFlags & ~oldFlags
      def removedFlags           = oldFlags & ~newFlags
      def changedFlags           = addedFlags | removedFlags

      def didAddFlag(f: Long)    = (addedFlags & f) != 0
      def didLoseFlag(f: Long)   = (removedFlags & f) != 0
      def didChangeFlag(f: Long) = didAddFlag(f) || didLoseFlag(f)

      override protected def participants: List[Any] = List(carrier, changedFlags)
      def carrierString = anyString(carrier)
      override def toString = {
        val plus = if (addedFlags == 0) "" else "+(" + flagsString(addedFlags) + ")"
        val minus = if (removedFlags == 0) "" else "-(" + flagsString(removedFlags) + ")"
        
        joinString(carrierString, plus, minus)
      }
    }
    trait SymFlagEvent extends SymEvent with FlagEvent[Symbol] {
      def carrier = sym
    }
    trait ModsFlagEvent extends FlagEvent[Modifiers] {
      def mods: Modifiers
      def carrier = mods
    }
    
    trait ReferencesInfo[T] {
      def references: List[T]
    }
    
    trait SymbolReferencesEvent extends ReferencesInfo[Symbol]
    
    trait TreeReferencesEvent extends ReferencesInfo[Tree]

    /** Filters limit the event stream.
     */
    trait AbsFilter extends (Event => Boolean) {
      self: Filter =>

       // A short description of this filter
      def tag: String = ""
      def apply(ev: Event): Boolean

      def unary_! : Filter              = Filter.not(this)
      def and(other: Filter): Filter    = Filter.and(this, other)
      def or(other: Filter): Filter     = Filter.or(this, other)
      def butNot(other: Filter): Filter = Filter.and(this, !other)
      
      override def toString = tag
    }
    abstract class FilterCompanion {
      val empty: Filter = apply(_ => true)
      
      def decompose[R](f: Event => R): (Filter, Event => R) = f match {
        case pf: PartialFunction[_, R]  => (apply(pf isDefinedAt _), pf apply _)
        case _                          => (empty, f)
      }

      def and(filters: Filter*): Filter
      def or(filters: Filter*): Filter
      def not(filter: Filter): Filter
      def apply(f: Event => Boolean): Filter
      def pf(pf: Event =>? Boolean): Filter = apply(x => (pf isDefinedAt x) && pf(x))
    }
    
    /** Hooks dip into the event stream.
     */
    trait AbsHook extends PartialFunction[Event, EventResponse] {
      self: Hook =>
      
      def action(ev: Event): EventResponse
      def start(): this.type
      def stop(): this.type
      def show(ev: Event): Unit = ()

      def startBlock: Unit
      def endBlock: Unit
      
      def hooking[T](body: => T): T = {
        try {
          start()
          body
        }
        finally stop()
      }

      final def applyIfDefined(ev: Event) = if (isDefinedAt(ev)) apply(ev)
      final def isDefinedAt(ev: Event)    = filter(ev)
      final def apply(ev: Event)          = action(ev)
      
      private var _filter: Filter = Filter.empty
      def filter: Filter = _filter
      def filterBy(p: Filter): this.type = {
        _filter = _filter and p
        this
      }
      private def filterString = if (filter.tag == "") "" else " with filter " + filter.tag
      override def toString    = "Hook filterd by " + filterString
    }
    
    abstract class HookCompanion {
      def apply(f: Event =>? Unit): Hook 
    }

    // Normal way of informing about an event
    // Contains no information for visualization
    @elidable(2500)
    def <<(ev: Event): EventResponse
    
    // Event starting block
    @elidable(2500)
    def <<<(ev: Event): EventResponse
    
    // Event ending block
    @elidable(2500)
    def >>(ev: Event): EventResponse
    
    // Event ending block without actual handling of the event
    @elidable(2500)
    def >>>(ev: Event): EventResponse
  }
}
