package scala_fp.handle_error

import java.util.regex.{PatternSyntaxException, Pattern}

/**
 * Created by liguang on 15/1/11.
 * None 操作
 */

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }
}

case class Some[A](get: A) extends Option[A]

case object None extends Option[Nothing]


object One {

  def failingFn(i: Int): Int = {
    val x: Int = throw new Exception("Fail!")
    try {
      val y = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    //    果然精简
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    //    if (xs.isEmpty)
    //      None
    //    else {
    //      val m = mean(xs)
    //      Some(math.sqrt(xs.map(x => math.pow(x - m.getOrElse(0.0), 2)).sum))
    //    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = a => a.map(f)

  //  如果一个函数返回Option  一般情况下 所有的调用者都必须去显式检查Option情况
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] = {
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)
  }

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = {
    // 等价于 oa flatMap (a => ob map (b => f(a,b)))
    for {
      a <- oa
      b <- ob
    } yield f(a, b)
  }

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher("s1"), mkMatcher("s2"))((a, b) => a(s) && b(s))

  // a里面如果有一个None 就直接返回None
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(v => sequence(t).map(l => v :: l))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    //    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
    traverse(a)(v => v)
  }

  def main(args: Array[String]) {
    //    println(mean(List(1, 2, 3, 4)))
    //    println(mean(List(1, 2, 3, 4)).map(_ + 1))

    case class Employee(name: String, department: String)

    val employeesByName: Map[String, Employee] =
      List(Employee("Alice", "R&D"), Employee("Bob", "Accounting")).
        map(e => (e.name, e)).toMap

    val dept = employeesByName.get("Bob").map(_.name)
    println(dept)

    println(variance(List()))

    val dept2 = employeesByName.get("Joe").map(_.department).
      filter(_ != "Accounting").getOrElse("Default Dept")

    dept.getOrElse(throw new Exception("FAIL"))

    println(bothMatch("s1", "s2", "iloves1ands2"))

    println(sequence(List(Some(1), Some(2), Some(3), Some(4), None)))
    println(sequence2(List(Some(1), Some(2), Some(3), Some(4))))
    println(sequence2(List(Some(1), Some(2), Some(3), Some(4), None)))

    println(traverse(List(1, 2, 3, 4))(Some(_)))
  }
}
