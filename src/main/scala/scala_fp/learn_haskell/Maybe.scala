package scala_fp.learn_haskell

/**
 * Created by liguang on 15/2/3.
 */
trait Maybe[+A]

object None extends Maybe[Nothing]

case class Just[A](v: A) extends Maybe[A]

trait Either[+A, +B]

case class Left[A](v: A) extends Either[A, Nothing]

case class Right[B](v: B) extends Either[Nothing, B]


trait TrafficLight

object Red extends TrafficLight

object Yellow extends TrafficLight

object Green extends TrafficLight


trait Eq[A] {
  def ===(a: A, b: A): Boolean
}

object Eq {
  implicit val TrafficLightEq = new Eq[TrafficLight] {
    def ===(a: TrafficLight, b: TrafficLight): Boolean = (a, b) match {
      case (Red, Red) => true
      case (Yellow, Yellow) => true
      case (Green, Green) => true
      case (_, _) => false
    }
  }

  implicit val IntEq = new Eq[Int] {
    def ===(a: Int, b: Int): Boolean = a == b
  }
}


object TestDa {
  def main(args: Array[String]) {
    val str: Maybe[String] = Just("s")
    val s = str match {
      case Just(v) => v
      case None => "none"
    }
    print(s)
    val e: Either[Int, Int] = Right(1)
    println(e match {
      case Left(v) => v
      case Right(v) => v
    })

    testEq(Red, Yellow)
    testEq(1, 1)
  }

  def testEq[A: Eq](a: A, b: A): Boolean = implicitly[Eq[A]].===(a, b)
}


case class Person(firstName: String, lastName: String, age: Int)