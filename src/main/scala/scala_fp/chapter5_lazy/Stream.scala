package scala_fp.chapter5_lazy

import scala.collection.mutable.ListBuffer

/**
 * Created by liguang on 15/1/13.
 * Stream
 */

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    val buf = new ListBuffer[A]
    def go(s: Stream[A]): List[A] = {
      if (s.isEmpty) buf.toList
      else {
        buf += s.uncons.get._1
        go(s.uncons.get._2)
      }
    }
    go(this)
  }

  def take(n: Int): Stream[A] = {
    val buf = new ListBuffer[A]

    def take1(s: Stream[A], n: Int): Stream[A] = {
      if (n == 0) Stream(buf.toList: _*)
      else if (s.isEmpty) Stream(buf.toList: _*)
      else {
        buf += s.uncons.get._1
        take1(s.uncons.get._2, n - 1)
      }
    }
    take1(this, n)
  }
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    def uncons = None
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
}

object App {
  def main(args: Array[String]) {
    val s = Stream(1, 2, 3, 4, 5)
    println(s.toList)
    println(s.take(3).toList)
  }
}
