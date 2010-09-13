/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic._
import scala.reflect.ClassManifest
import mutable.ListBuffer
import annotation.tailrec
import annotation.unchecked.{ uncheckedVariance => uV }

trait TraversableExtensions[+A, +Repr] {
  self: TraversableLike[A, Repr] =>
  
  import Traversable.breaks._
  
  private def repSequence(
    f: Traversable[A @uV] => (Traversable[A @uV], Traversable[A @uV]),
    extras: Traversable[A @uV]*): List[Repr] = {
      
    def mkRepr(xs: Traversable[A @uV]): Repr = newBuilder ++= xs result
    val bb = new ListBuffer[Repr]
  
    @tailrec def loop(xs: Repr): List[Repr] = {
      val seq = toCollection(xs)
      if (seq.isEmpty)
        return (bb ++= (extras map mkRepr)).result
      
      val (hd, tl) = f(seq)
      bb += mkRepr(hd)
      loop(mkRepr(tl))
    }
    
    loop(self.repr)
  }

  def inits: List[Repr]   = repSequence(x => (x, x.init), Nil)
  def tails: List[Repr]   = repSequence(x => (x, x.tail), Nil)
  def cluster[A1 >: A : Equiv]: List[Repr] =
    repSequence(x => x.span(y => implicitly[Equiv[A1]].equiv(y, x.head)))
}
