/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package event

import symtab.{ Flags, SymbolTable }
import io.{ AbstractFile, Path }


trait EventsGlobal extends EventsSymbolTable {
  outer: Global =>
  
  /*val EV: EventModel {
    val global: outer.type
  }*/
  
  abstract class EventModel extends super.EventModel {
    model =>

    val global: outer.type
    var posOK = false   // XXX avoiding startup deadlock in posAssigner
    
    type Context         = global.analyzer.Context
    type ImportInfo      = global.analyzer.ImportInfo
    type CompilationUnit = global.CompilationUnit

    def currentRun       = global.currentRun
    def currentUnit      = if (currentRun == null) null else currentRun.currentUnit
    def currentPos: Position = 
      if (!isInitialized || !posOK) NoPosition
      else Option(global.posAssigner.pos) getOrElse NoPosition
    
    val TypeManifest     = manifest[Type]
    val SymbolManifest   = manifest[Symbol]
    val TreeManifest     = manifest[Tree]
    val NameManifest     = manifest[Name]
    val PositionManifest = manifest[Position]
    
    override protected def involvedNames(entity: Any): List[Name] = entity match {
      case x: Context       => involvedNames(x.scope)
      case x: ImportInfo    => x.allImportedSymbols flatMap involvedNames
      case _                => super.involvedNames(entity)
    }

    override protected def anyStringInternal(x: Any): String = x match {
      case x: Context    => "Context for " + anyStringInternal(x.tree)
      case x: ImportInfo => x.tree.toString + " (" + x.allImportedSymbols.size + " imported symbols)"
      case _             => super.anyStringInternal(x)
    }

    
    case class NewTopLevelModule(sym: Symbol) extends SymEvent {
      def tag = "newTopLevelModule"
    }
    case class NewTopLevelClass(sym: Symbol) extends SymEvent {
      def tag = "newTopLevelClass"
    }    
    case class TypedNode(context: Context, value1: Tree, value2: Tree) extends BinaryEvent[Tree] {
      def tag = "typedNode"
      def oldTree = value1
      def typedTree = value2
      def binaryOp = "typed"
    }
    case class NewImport(imp: ImportInfo) extends Event {
      def tag = "newImport"
      protected def participants: List[Any] = List(imp)
    }
    case class NewContext(context: Context) extends Event {
      def tag = "newContext"
      protected def participants: List[Any] = List(context)
    }
    case class LoadedClassFile(file: AbstractFile, sym: Symbol) extends SymEvent {
      def tag = "loadedClassFile"
    }
  }
}
