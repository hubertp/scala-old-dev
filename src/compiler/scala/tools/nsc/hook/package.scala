/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */
 
package scala.tools.nsc

import symtab.Symbols
import interpreter.stripWrapperGunk
import scala.reflect.NameTransformer

package object hook {
  type AnySym     = Symbols#Symbol
  
  private[hook] def shortName(s: AnySym)  = stripWrapperGunk(s.decodedName)
  private[hook] def longName(s: AnySym)   = stripWrapperGunk(NameTransformer.decode(s.fullName('.')))
  private[hook] def tos(method: AnySym) = {
    val classString = longName(method.owner)
    if (method.isConstructor) classString
    else classString + "." + shortName(method)
    
    // if (method.isConstructor) "new " + classString
    // else classString + "." + shortName(method)
  }
}
