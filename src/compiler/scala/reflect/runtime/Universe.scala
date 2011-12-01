package scala.reflect
package runtime

import internal.{SomePhase, NoPhase, Phase, TreeGen}

/** The universe for standard runtime reflection from Java.
 *  This type implements all abstract term members in internal.SymbolTable.
 *  It also provides methods to go from Java members to Scala members,
 *  using the code in JavaConversions.
 */
class Universe extends SymbolTable with internal.transform.Transforms with EventUniverseStub {
  
  type AbstractFileType = AbstractFile
  
  def picklerPhase = SomePhase
    
  val gen = new TreeGen { val global: Universe.this.type = Universe.this }
  
  def settings = new Settings
  def forInteractive = false
  def forScaladoc = false
  
  val phaseWithId: Array[Phase] = Array(NoPhase, SomePhase)
  val currentRunId = 1 // fake a run id so that it is different from NoRunId
  phase = SomePhase // set to a phase different from NoPhase
  
  def log(msg: => AnyRef): Unit = println(" [] "+msg)  
  
  type TreeCopier = TreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier
    
  def focusPos(pos: Position) = pos
  def isRangePos(pos: Position) = false
  def showPos(pos: Position) = "<unknown position>"
    
  type Position = String // source file?
  val NoPosition = ""
  
  // establish root association to avoid cyclic dependency errors later
  classToScala(classOf[java.lang.Object]).initialize
}

trait EventUniverseStub {
  outer: Universe =>
  
  val EV = new EventModel {
    // Some of the stuff would need to have to be moved to reflect.interal to correctly initialise EV
    val global: outer.type = outer
    type EventResponse = Unit
    type CompilationUnit = NoCompilationUnit.type
    type Phase = internal.Phase
    
    def <<(ev: Event): Unit =  {}
    def >>(ev: Event): Unit =  {}
    def >>>(ev: Event): Unit = {}
    def <<<(ev: Event): Unit = {}
    val Filter: FilterCompanion = null
    val Hook: HookCompanion = null
    val NameManifest: scala.reflect.Manifest[outer.Name] = null
    val NoPhase: Phase = SomePhase
    val NoResponse: EventResponse = ()
    val PositionManifest: scala.reflect.Manifest[outer.Position] = manifest[Position]
    val SymbolManifest: scala.reflect.Manifest[outer.Symbol] = manifest[Symbol]
    val TreeManifest: scala.reflect.Manifest[outer.Tree] = manifest[Tree]
    val TypeManifest: scala.reflect.Manifest[outer.Type] = manifest[Type]
    def anyString(x: Any): String = ""
    def currentPhase = phase
    def currentPos: outer.Position = outer.NoPosition
    def currentUnit: CompilationUnit = NoCompilationUnit
    def eventsOn: Boolean = false
    def flagsString(flags: Long): String = internal.Flags.flagsToString(flags)
    def posString(pos: outer.Position): String = ""
    def formatTypeString(tpe: Type): String = "(not implemented)"
      
    object NoCompilationUnit {
      def source: Any = null
    }
  }
}
