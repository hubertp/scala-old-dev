/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package event

import symtab.{ SymbolTable, Flags }
import Flags.flagsToString
import interpreter.IMain.stripString

trait EventStrings {
  self: SymbolTable with EventsSymbolTable with symtab.Positions =>
  
  trait Strings {
    self: EventModel =>
    
    def stripInterpreterWrapping = true
    
    /** Avoiding cycles when creating a String out of something
     *  induces events.
     */
    private var _eventsOn: Boolean = true
    def eventsOn = _eventsOn
    def withNoEvents[T](body: => T): T = {
      val saved = _eventsOn
      try {
        _eventsOn = false
        if (saved && isDebug)
          log("Disabling compiler event stream.")
        body
      }
      finally {
        if (saved && isDebug)
          log("Reenabling compiler event stream.")
        _eventsOn = saved
      }
    }
    final def anyString(x: Any): String = {
      val f: String => String = 
        if (stripInterpreterWrapping) stripString
        else identity[String]
      
      try f(
        if (eventsOn) withNoEvents(anyStringInternal(x))
        else anyStringInternal(x)
      )
      catch {
        case x @ (_: AssertionError | _: Exception) => "<error: " + x.getMessage + ">"
      }
    }

    protected def anyStringInternal(x: Any): String = x match {
      case null           => "null"
      case x: String      => x
      case x: JClass[_]   => x.getName split '.' last
      case x: Name        => x.toString
      case x: Symbol      => symString(x)
      case x: Type        => typeString(x)
      case x: Tree        => treeString(x)
      case x: Long        => flagsString(x)
      case x: Position    => posString(x)
      case x: Scope       => "Scope(" + x.toList.size + " members)"
      case x: BaseTypeSeq => (x.toList map anyStringInternal).mkString("BTS(", ", ", ")")
      case x: List[_]     => anyStringInternalList(x)
      case x              => anyStringInternal(x.asInstanceOf[AnyRef].getClass)
    }
    
    private def anyStringInternalList(x: List[Any]): String = {
      x.map(anyStringInternal).mkString("(", ",", ")")
    }
    def posString(pos: Position): String = {
      if (pos == NoPosition) ""
      else try { "(at " + pos.source + " line " + pos.line + ")" }
      catch    { case _: UnsupportedOperationException => "" }
    }
    def classString(clazz: JClass[_]): String = clazz.getName split '.' last
    def symString(sym: Symbol): String        = sym.nameString
    def flagsString(flags: Long): String      = flagsToString(flags)
    def typeString(tpe: Type): String         = "[" + tpe.kind + ": " + (tpe match {
      case x:TypeRef                   => x.safeToString
      case x:MethodType                => x.safeToString
      case x:PolyType                  => x.safeToString
      case x if (x.typeSymbol != null) => symString(x.typeSymbol)
      case _                           => classString(tpe.getClass)
    }) + "]"
    def formatTypeString(tpe: Type): String   = "(not implemented)"
    def treeString(tree: Tree): String   = treeName(tree) match {
      case None     => classString(tree.getClass)
      case Some(name)     => name
    }
  
    def treeName(tree: Tree): Option[String] = tree match {
      case x if x.symbol != null && x.symbol != NoSymbol    => Some(x.symbol.name.toString)
      case x: DefTree               => Some(x.name.toString)
      case x: RefTree               => Some(x.name.toString)
      case x: Literal               => Some(x.value.stringValue)
      case x: TypeApply             => Some(x.fun.toString + x.args.map(_.toString).mkString("[", ",", "]"))
      case _: TermTree => Some(tree.toString)
      case _: TypTree =>
        tree match {
          case ExistentialTypeTree(tpt, _)  => treeName(tpt)
          case AppliedTypeTree(tpt, _)      => treeName(tpt)
          case _                            => None
        }
      case _ => None
    }
  }
}
