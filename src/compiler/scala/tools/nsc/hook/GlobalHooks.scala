// /* NSC -- new Scala compiler
//  * Copyright 2005-2010 LAMP/EPFL
//  * @author  Paul Phillips
//  */
// 
// package scala.tools.nsc
// package hook
// 
// import util.Position
// import util.ScalaClassLoader.URLClassLoader
// import ast.Trees
// import symtab.{ Symbols, Flags }
// import Flags.flagsToString
// import GlobalHooks.knownHooks
// 
// trait GlobalHooks {
//   self =>
//   
//   val global: Global
//   import global.{ Position => _, _ }
//   
//   private var lastSeenHook: String = null
//   private def hookSetting          = settings.Yhookjvm.value
//   private def hookUpToDate         = hookSetting == lastSeenHook
// 
//   protected var userHook: UserHook    = null
// 
//   private def translateHookArg(): UserHook = {
//     if (hookSetting == "" || hookSetting == null)
//       return null
// 
//     val (hookArg, rest) = (hookSetting indexOf ':') match {
//       case -1   => (hookSetting, "")
//       case idx  => (hookSetting take idx, hookSetting drop (idx + 1))
//     }
//     knownHooks foreach {
//       case (knownHook, creator) if knownHook == hookArg =>
//         return creator(global, rest)
//       case _ => ()
//     }
//     
//     try byName[UserHook](hookArg) withArgs rest
//     catch { case x => 
//       global.warning("Failed to install hook '%s': %s".format(hookSetting, x.getMessage))
//       x.printStackTrace()
//       null
//     }
//   }
// 
//   private val loader = new URLClassLoader(classPath.asURLs, this.getClass.getClassLoader)
//   private def byName[T](name: String): T = {
//     val clazz = Class.forName(name, true, loader)
//     val constructor = clazz.getConstructor(global.getClass)
//     val res = constructor.newInstance(global)
//     
//     res.asInstanceOf[T]
//   }
//   
//   def updateHook(): Unit = {
//     if (hookUpToDate)
//       return
//     
//     lastSeenHook = hookSetting
//     userHook = translateHookArg()
//     global.log("Set " + userHook + " as hook.")
//   }
//   
//   def callHook(pos: Position, arg: HookPoint): Unit = {
//     updateHook()
//     if (userHook == null)
//       return
//     
//     userHook.hook(pos, arg)
//   }
// }
// 
// object GlobalHooks {
//   type Creator = (Global, String) => UserHook
//   
//   class FlagHook(global: Global) extends UserHook(global) {    
//     private lazy val flagsUtil = new util.FlagsUtil(Flags)
//     private lazy val flagsMap: Map[String, Long] = {
//       val namesZip = flagsUtil.reflectiveFlagNames.zipWithIndex
//       val m: List[(String, Long)] = namesZip flatMap {
//         case (flags, idx) => flags map (_ -> (1L << idx))
//       }
//       m.toMap
//     }
//     
//     def givenMask = {
//       val xs = (args split ":") flatMap (flagsMap get _)
//       xs.foldLeft(0L)(_ | _)
//     }
//     private def isHit(flag: Long) = {
//       isPhase("typer") && ((flag & givenMask) != 0) // XXX
//     }
// 
//     def hook(pos: Position, arg: HookPoint) = {
//       // println("hook: givenMask = " + givenMask + ", arg = " + arg)
//       arg match {
//         case SetFlag(sym, flag) if isHit(flag) =>
//           show(pos, arg)
//         case SetModFlag(sym, flag) if isHit(flag) =>
//           show(pos, arg)
//         case _ =>
//       }
//     }
//   }
// 
// 
