object Test {
  def f1(a: Any)  = a match {
    case _: Array[AnyRef]         => 1
    case _: Array[String]         => 2
    case _: Array[Array[AnyRef]]  => 3
    case _: Array[Array[String]]  => 4
    case _: Array[Float]          => 5
    case _: Array[_]              => 6
    case _                        => 7
  }
  def f2(a: Any) = a.isInstanceOf[Array[AnyRef]]
  def f3(a: Any) = a.isInstanceOf[Array[String]]
  def f4(a: Any) = a.isInstanceOf[Array[Array[AnyRef]]]
  def f5(a: Any) = a.isInstanceOf[Array[Array[String]]]
  def f6(a: Any) = a.isInstanceOf[Array[Float]]
  def f7(a: Any) = a.isInstanceOf[Array[_]]

  def to_s(x: AnyRef) = x.getClass.getName
  
  val a1 = Array(new AnyRef)
  val a2 = Array("a")
  val a3 = Array(a1)
  val a4 = Array(a2)
  val a5 = Array(5.0f)
  val a6 = Array(new util.Random)
  val a7 = "abcdefg"
  
  def main(args: Array[String]): Unit = {
    List(a1, a2, a3, a4, a5, a6, a7) foreach { x => 
      println(List(to_s(x), f1(x), f2(x), f3(x), f4(x), f5(x), f6(x), f7(x)) mkString " ")
    }
  }
}