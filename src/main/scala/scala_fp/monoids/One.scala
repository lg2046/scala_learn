package scala_fp.monoids

/**
 * Created by liguang on 15/2/6.
 * monoid 1
 */

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  implicit val intAdditionMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2

    override def zero = 0
  }

  val intMultiplicationMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2

    override def zero = 1
  }

  val booleanOrMonoid = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2

    override def zero = false
  }

  val booleanAndMonoid = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2

    override def zero = true
  }

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero = ""
  }

  implicit def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero = Nil
  }

  implicit def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2

    override def zero = identity
  }

  def wordsMonid = new Monoid[String] {
    override def op(a1: String, a2: String): String = (a1, a2) match {
      case ("", x) => x.trim
      case (x, "") => x.trim
      case _ => a1.trim + " " + a2.trim
    }

    override def zero: String = ""
  }


  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  // Now we can have both monoids on hand:
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]

  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def concatenate[A](as: List[A], m: Monoid[A]) = as.foldLeft(m.zero)(m.op)

  def foldMap2[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, x) => m.op(acc, f(x)))

  def foldRight[A, B](as: List[A], z: B, f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)
}


object One {
  def main(args: Array[String]) {
    println(implicitly[Monoid[List[Int]]].op(List(1, 2), List(2, 3)))

    println(Monoid.optionMonoid[Int].op(Option(1), Option(2)))
    println(Monoid.dual(Monoid.optionMonoid[Int]).op(Option(1), Option(2)))

    println(Monoid.endoMonoid[Int].op((x: Int) => -x, (x: Int) => x + 1)(5))
    val strMonoid = Monoid.wordsMonid
    println(strMonoid.op("Hic", strMonoid.op("est ", "chorda ")))
    println(strMonoid.op("Hic ", strMonoid.op(" est", "chorda")))

    Monoid.concatenate(List("a", "b", "c"), Monoid.stringMonoid)

    Monoid.concatenate(List("a", "b", "c"), Monoid.wordsMonid)
    //    Monoid.foldMap1(List("2", "2", "3"), Monoid.intAdditionMonoid)((x) => x.toInt)

    implicit class StringToInt(str: String) {
      def toInt2 = Integer.parseInt(str)
    }

    implicit class DoubleOps2(d: Double) {
      def ~=(d2: Double, precision: Double = 0.000001) =
        if ((d - d2).abs < precision) true else false
    }
  }

  def getClassAsString(x: Any): String = x match {
    case s: String => s + " is a String"
    case i: Int => "Int"
    case f: Float => "Float"
    case l: List[_] => "List"
//    case p: Person => "Person"
    case _ => "Unknown"
  }

}
