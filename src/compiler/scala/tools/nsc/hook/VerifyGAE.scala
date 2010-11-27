/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package event

import Properties.envOrElse

/** Creates a hook which checks the google app engine whitelist.
 */
object VerifyGAE {
  private def isSkip(s: String) = (s startsWith "#") || (s.trim == "")
  private val whiteURL  = "http://code.google.com/intl/en-US/appengine/docs/java/jrewhitelist.html"
  private val whiteFile = "scala/tools/nsc/event/resource/gae-whitelist.txt"
  // envOrElse("GAE_WHITELIST", "/scala/trunk/tools/gae-whitelist.txt")
  
  val ignorePackageNames = List("scala.swing")
  val jdkPackageNames    = List("java", "javax", "org.w3c.dom", "org.xml.sax")
  val whiteSet           = {
    val cl = util.ScalaClassLoader(classOf[Global].getClassLoader)
    val str = cl.stringResource(whiteFile)
    str.lines filterNot isSkip toSet
    // 
    // val file = io.File(whiteFile)
    // if (file.isFile) file.lines() filterNot isSkip toSet
    // else {
    //   Console.println("Unable to find '%s': set environment variable GAE_WHITELIST".format(whiteFile))
    //   Console.println("See " + whiteURL + " for expected contents.")
    //   Set()
    // }
  }
}
import VerifyGAE._

/** Without the override val global we get:
[scalacfork] /scala/trunk/src/compiler/scala/tools/nsc/hook/VerifyGAE.scala:37: error: type mismatch;
[scalacfork]  found   : VerifyGAE.this.global.EV.Hook
[scalacfork]  required: VerifyGAE.this.global.EV.Hook
[scalacfork]   def createHook(arg: String) = Hook({
[scalacfork]                                     ^
*/
class VerifyGAE(override val global: Global) extends UserHook(global) {
  import global.{ definitions, view, Symbol, EV }
  import EV._
  
  private def symbolSet(xs: List[String]): Set[Symbol] =
    xs map (x => definitions.getModule(x).tpe.typeSymbol) toSet

  private val jdkPackages = symbolSet(jdkPackageNames)
  private val fmt = "GAE violation at %po: %ev"
  private def show(ev: Event) = Console println (ev formattedString fmt)
  
  def isJDK(sym: Symbol)          = jdkPackages exists (sym hasTransOwner _)
  def isIgnore(sym: Symbol)       = ignorePackageNames exists (sym.enclosingPackage.fullName.toString startsWith _)
  def isWhitelisted(sym: Symbol)  = (
    whiteSet(sym.owner.fullName.toString takeWhile (_ != '$')) ||
    (sym.enclosingPackage.fullName.toString startsWith "javax.swing")
  )
  
  def createHook(arg: String) =
    Hook({ case ev @ CallMethod(from, to) if isJDK(to) && !isWhitelisted(to) && !isIgnore(from) => show(ev) })
}
