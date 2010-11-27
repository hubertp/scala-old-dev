// test script for compiler events

import scala.tools.nsc._
import event._
import symtab.Flags._
import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, HashSet }

:power
import global._
import EV._
import EVDSL._

object src {
  def sources(roots: String*): List[String] = {
    val lb = new ListBuffer[String]
    roots foreach { p =>
      io.Path(p).walk foreach { x =>
        if (x.isFile && x.hasExtension("scala", "java"))
          lb += x.path
      }
    }
    lb.toList.distinct
  }
  def cc(paths: String*) = {
    reporter.reset
    val run = new Run
    run compile sources(paths: _*)
    !reporter.hasErrors
  }

  val mutable = "/scala/trunk/src/library/scala/collection/mutable/"
  val patmat = "/scala/trunk/src/compiler/scala/tools/nsc/matching/"
  val global = "/scala/trunk/src/compiler/scala/tools/nsc/Global.scala"
}

class Logger(fmt: String) {
  def this() = this("[%ph] %ev %po")
  def show(ev: Event) = Console println (ev formattedString fmt)

  // or whatever you want to compile
  var toCompile: List[String] = List(src.global)
  
  def pf(fxn: Event =>? Boolean): Unit = apply(Filter pf fxn)
  def apply(filt: Filter): Unit = {
    val hook = Hook({ case ev if filt(ev) => show(ev) })
    hook hooking src.cc(toCompile: _*)
  }
}
val logger = new Logger()

def findLoads() = logger pf { case _: LoadedClassFile => true }
def findCalls(method: String) = {
  logger pf { case CallMethod(_, target) => target.name.toString == method }
}
// val CASEACCESSOR SUPERACCESSOR EXPANDEDNAME IMPLCLASS FINAL LAZY LIFTED
def findFlags(mask: Long) = logger(ev +/- mask)
def findImports() = logger pf { case _: ImportInfo => true }
def newTermSyms() = logger pf { case _: NewTermSymbol => true }
def newTypeSyms() = logger pf { case _: NewTypeSymbol => true }

def find(f: Filter) = logger(f)
def findLazy() = find(ev +/- LAZY and ph >= 4 and ph <= 9)
def findCaseAccessor() = findFlags(CASEACCESSOR)
def findLifted() = findFlags(LIFTED)
