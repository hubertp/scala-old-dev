package scala.tools.nsc
package event

trait GeneralEvents {
  self: symtab.SymbolTable with EventsSymbolTable =>
  
  trait ControlEvents {
    self: EventModel =>
      
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
  }
}