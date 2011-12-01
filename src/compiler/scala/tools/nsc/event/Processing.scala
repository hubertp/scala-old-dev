/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package event

import symtab.{ Flags, SymbolTable }
import scala.collection.{ mutable, immutable }

trait Processing {
  self: SymbolTable with EventsSymbolTable =>
  
  trait ProcessingUtil {
    self: EventModel =>

    object Gatherer { }
    class Gatherer[T: Manifest](f: T => Boolean) extends mutable.Set[T] {
      gatherer =>
    
      private val set = mutable.HashSet[T]()
      val tag         = "gatherer: " + classString(manifest[T].erasure)
    
      lazy val hook: Hook = Hook({
        case x if x matches f =>
          gatherer.set ++= x matching f
          NoResponse
      })

      def contains(key: T) = set contains key
      def iterator: Iterator[T] = set.iterator      
      def +=(elem: T): this.type = {
        set += elem
        this
      }
      def -=(elem: T): this.type = {
        set -= elem
        this
      }
    
      private var _enabled = true
      def isEnabled = _enabled
      def disable() = {
        removeHook(hook)
        _enabled = false
      }
      def enable() = {
        addHook(hook)
        _enabled = true
      }
      override def toString = {
        val prefix = if (isEnabled) "Enabled" else "Disabled"
        prefix + " " + tag + " with " + set.size + " elements."
      }
    }  
  }
}
