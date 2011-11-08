/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package event

import util.ScalaClassLoader.URLClassLoader
import symtab.Flags

abstract class UserHook(val global: Global) {
  def createHook(arg: String): global.EV.Hook
}

object HookLoader {  
  val flagsUtil = new util.FlagsUtil(Flags)
  val flagsMap: Map[String, Long] = {
    val namesZip = flagsUtil.reflectiveFlagNames.zipWithIndex
    val m: List[(String, Long)] = namesZip flatMap {
      case (flags, idx) => flags map (_ -> (1L << idx))
    }
    m.toMap
  }
  // 
  // class FlagHook(global: Global, args: String) extends UserHook {
  //   import global.EV.Hook
  //   
  //   val givenMask = (args split ":").flatMap(flagsMap get _).foldLeft(0L)(_ | _)
  //   private def isHit(flag: Long) = (flag & givenMask) != 0
  //   def hook: Hook = null
  // }
  
  val knownHooks: Map[String, UserHook] = Map()
}
import HookLoader._

trait HookLoader {
  val global: Global
  import global.{ definitions, settings, classPath, view, Symbol, EV }
  import EV._

  private val loader = new URLClassLoader(classPath.asURLs, this.getClass.getClassLoader)
  private def byName[T](name: String): T = {
    val clazz       = Class.forName(name, true, loader)
    val constructor = clazz.getConstructor(global.getClass)
    val res         = constructor.newInstance(global)
    
    res.asInstanceOf[T]
  }

  def createHookFromString(hookArgument: String): Hook = {
    if (hookArgument == "" || hookArgument == null)
      return null

    val (hookArg, rest) = (hookArgument indexOf ':') match {
      case -1   => (hookArgument, "")
      case idx  => (hookArgument take idx, hookArgument drop (idx + 1))
    }
    def finish(userHook: UserHook): Hook =
      (userHook createHook rest).asInstanceOf[Hook]

    knownHooks foreach {
      case (knownHook, action) if knownHook == hookArg =>
        return finish(action)
      case _ => ()
    }
    
    try finish(byName[UserHook](hookArg))
    catch { case x => 
      global.warning("Failed to install hook '%s': %s".format(hookArgument, x.getMessage))
      x.printStackTrace()
      null
    }
  }
}
