package scala_fp.functional_data_structure

import scala.annotation.tailrec

/**
 * Created by liguang on 15-1-4.
 * functional data structure
 */

//sealed 表示
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((acc, i) => f(i, acc))
  }

  def length[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  }

  @tailrec
  def foldLeft[A, B](l: List[A], a: B)(f: (B, A) => B): B = l match {
    case Nil => a
    case Cons(x, xs) => foldLeft(xs, f(a, x))(f)
  }

  def product(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_ * _)
  }

  def head[A](l: List[A]): A = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(h, _) => h
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  @tailrec
  def drop[A](n: Int, l: List[A]): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(n - 1, t)
    }

  }

  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => sys.error("set head on empty list")
    case Cons(_, t) => Cons(x, t)
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def flatten[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())(append)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((i, acc) => Cons(f(i), acc))
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    flatten(map(l)(f))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, List[A]())((i, acc) => if (f(i)) Cons(i, acc) else acc)
    //    l match {
    //      case Nil => Nil
    //      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    //    }
  }

  //  def zip[A,B](xa: List[A], xb: List[B]):List[(A,B)] = (xa, xb) match {
  //    case (Nil, _) => Nil
  //    case (_, Nil) => Nil
  //    case (x :: xs, y :: ys) => (x,y) :: zip(xs, ys)
  //  }

  def pairwise[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), pairwise(t1, t2)(f))
  }

  //  此时as是一个Seq[A]类型的
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }


}

object One {
  def main(args: Array[String]) {
    val example1 = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1, 2, 3, 4)
    println(List.sum(example1))
    println(List.sum(example2))
    println(List.product(List(1.0, 2.0, 3.0, 4.0)))

    println(List.tail(List(1, 2, 3)))
    println(List.tail(List(1, 2)))
    println(List.tail(List(1)))
    //    println(List.tail(List())) => will error

    println(List.init(List(1, 2, 3)))
    println(List.init(List(1, 2)))
    println(List.init(List(1)))
    //    println(List.init(List()))  => will error

    println(List.drop(2, List(1, 2, 3)))
    println(List.dropWhile(List(1, 2, 3, 4, 5, 6))(x => x < 3))

    println(List.append(List(1, 2, 3), List(0, 1, 2, 3)))

    println(List.reverse(List(0, 1, 2, 3)))
    println(List.map(List(0, 1, 2, 3))(_ + 1))
    println(List.map(List(0.0, 1.0, 2.0, 3.0))(_.toString))

    println(List.foldRight(List(1, 2, 3), List[Int]())(Cons(_, _)))

    println(List.flatten(List(List(0, 1, 2, 3), List(4, 5, 6), List(7, 8, 9))))

    println(List.filter(List(0, 1, 2, 3))(_ > 1))

    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))

    println(List.head(List(4, 5, 6)))

    println(List.pairwise(List(1, 2, 3), List(4, 5, 6))(_ + _))


    //    println(List(1, 2, 3, 4, 5) match {
    //      case Cons(x, Cons(2, Cons(4, _))) => x
    //      case Nil => 42
    //      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //      case Cons(h, t) => h + List.sum(t)
    //      case _ => 101
    //    })
  }
}

