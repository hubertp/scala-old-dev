// +  def promoteIndex(index: Int): Repr = {
// +  def dropIndex(index: Int): Repr =
// +  def extractIndex(index: Int): (A, Repr) = {
// +  def splitAround(index: Int): (Repr, A, Repr) = {
// +  def permutations: Iterator[Repr] = {
// +    def distinctIndices = toCollection(distinct) map (x => indexOf(x)) iterator;
// +    def mkRepr(xs: Traversable[A @uV]): Repr = newBuilder ++= xs result
// +  def inits: List[Repr]   = repSequence(x => (x, x.init), Nil)
// +  def tails: List[Repr]   = repSequence(x => (x, x.tail), Nil)
// +  def cluster[A1 >: A : Equiv]: List[Repr] =
// +  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
// +  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
// +  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
// +  def mapWith[A1 >: A, B](f: A1 => B): immutable.Map[A1, B] = {
// +  def multiMapWith[A1 >: A, B](f: A1 => B): immutable.Map[A1, List[B]] = {
//    def equiv(x: T, y: T): Boolean 
// +    def equiv(x: T, y: T) = cmp.compare(x, y) == 0
// +  def reference[T <: AnyRef] : Equiv[T] = new Equiv[T] {
// +    def equiv(x: T, y: T) = x eq y
// +  def universal[T] : Equiv[T] = new Equiv[T] {
// +    def equiv(x: T, y: T) = x == y
// +  def fromFunction[T](cmp: (T, T) => Boolean): Equiv[T] = new Equiv[T] {
// +    def equiv(x: T, y: T) = cmp(x, y)
// +  def by[T, S: Equiv](f: T => S): Equiv[T] =
// +  def apply[T: Equiv] : Equiv[T] = implicitly[Equiv[T]]

class A {
  override def equals(other: Any) = other.isInstanceOf[A]
  override def toString = "A"
}
class B {
  override def equals(other: Any) = other.isInstanceOf[B]
  override def toString = "B"
}

object EquivTest {  
  def to_s(xss: Seq[Seq[Any]]) = xss map (_.mkString) mkString " "
  
  def cluster() = {
    val xs = List("A".intern, "A".intern, new A, new A, new B, new B, new String("B"), new String("B"))

    List(
      to_s(xs.cluster) == "AA AA BB BB",
      to_s(xs cluster Equiv.reference) == "AA A A B B B B",
      to_s(xs cluster (Equiv by ((x: Any) => x.toString))) == "AAAA BBBB"
    )
  }
  
  def compares() = {
    val xs = List("1", "9", "5", "123", "99", "987", "555")
    
    List(
      (xs maxBy (_.toInt)) == "987",
      (xs minBy (_.toInt)) == "1",
      (xs mapWith (_.toInt / 3) get ("9")) == Some(3),
      (xs collectFirst { case x if x.last == '7' => x.toInt }) == Some(987)
    )
  }
  
  def run() = {
    cluster() foreach assert
    compares() foreach assert
  }
}

object SeqTest {
  val letters = "abcdef"
  
  def indexes() = List(
    (letters promoteIndex 3) == "dabcef",
    (letters dropIndex 3) == "abcef",
    (letters extractIndex 3) == ('d', "abcef"),
    (letters splitAround 3) == ("abc", 'd', "ef")
  )

  def seqs() = List(
    letters.permutations.size == 720,
    letters.permutations.toList.distinct.size == 720,
    "aaaaaaa".permutations.size == 1,
    (letters.tails mkString "-") == "abcdef-bcdef-cdef-def-ef-f-",
    (letters.inits mkString "-") == "abcdef-abcde-abcd-abc-ab-a-"
  )
  
  def run() = {
    indexes() foreach assert
    seqs() foreach assert
  }
}

object Test {
  
  def main(args: Array[String]): Unit = {
    EquivTest.run()
    SeqTest.run()
  }
}
