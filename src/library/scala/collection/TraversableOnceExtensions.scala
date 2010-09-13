/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import mutable.ListBuffer
import annotation.unchecked.{ uncheckedVariance => uV }

trait InvariantTraversableOnceExtensions[A] {
  self: TraversableOnce[A @uV] =>
  
  def mapWith[B](f: A => B): immutable.Map[A, B] = {
    val b = immutable.Map.newBuilder[A, B]
    for (x <- this) b += ((x, f(x)))
    b.result
  }
  
  def multiMapWith[B](f: A => B): immutable.Map[A, List[B]] = {
    val buf = new mutable.HashMap[A, ListBuffer[B]]
    for (x <- self)
      if (buf contains x) buf(x) += f(x)
      else buf(x) = ListBuffer(f(x))

    val b = immutable.Map.newBuilder[A, List[B]]
    for ((k, v) <- buf)
      b += ((k, v.toList))
  
    b.result
  }
}

trait TraversableOnceExtensions[+A] extends InvariantTraversableOnceExtensions[A @uV] {
  self: TraversableOnce[A @uV] =>
  
  /** Finds the first element of the $coll for which the given partial
   *  function is defined and returns it after applying the partial function. 
   *
   *  @param pf   the partial function
   *  @return     an option value containing pf applied to the first
   *              value for which it is defined, or `None` if none exists.
   */
  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    for (x <- self.toIterator) {
      if (pf isDefinedAt x)
        return Some(pf(x))
    }
    None
  }
  
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxBy")
    
    reduceLeft((x, y) => if (cmp.gteq(f(x), f(y))) x else y)
  }
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxBy")
    
    reduceLeft((x, y) => if (cmp.lteq(f(x), f(y))) x else y)
  }
}
