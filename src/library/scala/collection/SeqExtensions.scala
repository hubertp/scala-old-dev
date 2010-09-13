/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

trait SeqExtensions[+A, +Repr] {
  self: SeqLike[A, Repr] =>
  
  def promoteIndex(index: Int): Repr = {
    val it   = iterator drop index
    
    newBuilder += it.next ++= (iterator take index) ++= it result
  }
  def dropIndex(index: Int): Repr =
    extractIndex(index)._2

  def extractIndex(index: Int): (A, Repr) = {
    val it   = iterator drop index
    
    (it.next, newBuilder ++= (iterator take index) ++= it result)
  }

  def splitAround(index: Int): (Repr, A, Repr) = {
    val (xs, ys) = splitAt(index)
    val it = toCollection(ys).iterator
    
    (xs, it.next, newBuilder ++= it result)
  }
  
  def permutations: Iterator[Repr] = {
    def distinctIndices = toCollection(distinct) map (x => indexOf(x)) iterator;
    
    if (isEmpty) Iterator.empty
    else if (thisCollection.tail.isEmpty) Iterator(repr)
    else distinctIndices map (thisCollection splitAround _) flatMap {
      case (front, el, back) => (front ++ back).permutations map (newBuilder += el ++= _ result)
    }
  }
}
