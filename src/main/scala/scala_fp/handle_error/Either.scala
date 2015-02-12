package scala_fp.handle_error

/**
 * Created by liguang on 15/1/12.
 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either2 {
  def mean(xs: Seq[Int]): Either[Exception, Double] = {
    try {
      Right(xs.sum / xs.length)
    }
    catch {
      case e: Exception => Left(e)
    }
  }

  def main(args: Array[String]) {
    println(mean(List(1, 2)))
    println(mean(Nil))
  }
}
