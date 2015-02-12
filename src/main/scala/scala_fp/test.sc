//object Test {
//
//  sealed trait Rist[+A]
//
//  case object Nil extends Rist[Nothing]
//
//  case class Cons[+A](head: A, tail: List[A]) extends Rist[A]
//
//  println(Rist[Int]())
//
//}
//
//implicit val intOrd = new Ordering[Int] {
//  override def compare(x: Int, y: Int): Int =
//    if (x > y) 1
//    else if (x == y) 0
//    else -1
//}

def qsort[T](list: List[T])(implicit conv: T => Ordered[T]): List[T] = list match {
  case Nil => Nil
  case x :: xs =>
    val (before, after) = xs.partition(_ < x)
    qsort(before) ++ (x :: qsort(after))
}


qsort(List(1, 2, 3, 2, 1, 5, 4, 3, 2, 1))

scala.Predef

def lift2[A, B, C](f: (A, B) => C)(oa: Option[A])(ob: Option[B]): Option[C] = (oa, ob) match {
  case (Some(a), Some(b)) => Some(f(a, b))
  case _ => None
}

def add(a: Int, b: Int) = a + b

lift2(add)(Option(10))(None)