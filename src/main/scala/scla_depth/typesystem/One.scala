package scla_depth.typesystem

class Outer {

  trait Inner

  def y = new Inner {}

  def foo(x: Inner) = null

  def bar(x: Outer#Inner) = null
}

object Resources {
  type Resource = {
    def close(): Unit
  }

  def closeResource(r: Resource) = r.close()
}

//class A {
//  def foo[T >: List[Int]](a: T) = a
//}

class A {
  def count[T <: Traversable[Int]](a: T) = a.foldLeft(0)(_ + _)
}

trait Function[-Arg, +Return] {
  def apply(arg: Arg): Return
}


object Outer {
  def main(args: Array[String]) {
    //    val x = new Outer
    //    val y = new Outer
    //
    //    x.y

    val foo = new Function[Any, String] {
      override def apply(arg: Any): String = "hello i received a " + arg
    }

    val bar: Function[String, Any] = foo
  }
}