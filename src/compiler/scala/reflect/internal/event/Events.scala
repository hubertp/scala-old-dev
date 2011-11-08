package scala.reflect
package internal
package event

trait Events extends EventsUniverse {
  outer: SymbolTable =>
  
  val EV: EventModel {
    val global: outer.type
  }
  
  abstract class EventModel extends super.EventModel {
    case class SetFlag(sym: Symbol, oldFlags: Long, newFlags: Long, mask: Long) extends SymFlagEvent {
      def tag = "setFlag"
      override def eventString = joinString(sym, "+" + flagsString(mask))
    }
    case class ClearFlag(sym: Symbol, oldFlags: Long, newFlags: Long, mask: Long) extends SymFlagEvent {
      def tag = "clearFlag"
      override def eventString = joinString(sym, "-" + flagsString(mask))
    }
    case class SetModFlag(mods: Modifiers, oldFlags: Long, newFlags: Long, mask: Long) extends ModsFlagEvent {
      def tag = "setModFlag"
    }
    case class ClearModFlag(mods: Modifiers, oldFlags: Long, newFlags: Long, mask: Long) extends ModsFlagEvent {
      def tag = "clearModFlag"
    }
    case class NewTree(tree: Tree) extends TreeEvent {
      def tag = "newTree"
    }
    case class NewSym(sym: Symbol) extends SymEvent {
      def tag = "newSym"
    }
    case class CallMethod(value1: Symbol, value2: Symbol) extends TwoSymEvent {
      def tag          = "call"
      def caller       = value1.enclClass
      def callee       = value2.enclClass
      def sourceMethod = value1
      def targetMethod = value2
      // def exprType     = value2.tpe.resultType
      def binaryOp     = "->"
      
      private def callString(m: Symbol) = anyString(m.enclClass) + "." + anyString(m)
      override def eventString = joinString(callString(sourceMethod), binaryOp, callString(targetMethod))
    }
    case class SubTypeCheck(value1: Type, value2: Type, result: Boolean) extends TwoTypeEvent {
      def tag      = "subTypeCheck"
      def lhs      = value1
      def rhs      = value2
      def binaryOp = "<:<"
    }
    case class SymSetInfo(sym: Symbol, info: Type) extends SymEvent {
      def tag = "symSetInfo"
      override def eventString = {
        val elem = if (info.isComplete) info else "<incomplete>"
        joinString(sym, "set info", elem)
      }
    }
    
    case class NewTermSymbol(sym: TermSymbol) extends SymEvent {
      def tag = "newTermSymbol"
    }
    case class NewTypeSymbol(sym: TypeSymbol) extends SymEvent {
      def tag = "newTypeSymbol"
    }
  }
}