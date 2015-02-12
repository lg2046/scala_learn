package scala_fp.chapter1

import scala.annotation.tailrec

/**
 * Created by liguang on 15-1-4.
 * chapter 1
 */
case class Player(name: String, score: Int)


object One {
  def printWinner(p: Player) = {
    println(p.name + " is the winner!")
  }

  def declareWinner(p1: Player, p2: Player) = {
    printWinner(winner(p1, p2))
  }

  def winner(p1: Player, p2: Player) = {
    if (p1.score > p2.score) p1 else p2
  }

  //  def main(args: Array[String]) {
  //    val p1 = new Player("andy", 10)
  //    val p2 = new Player("fly", 10)
  //
  //    declareWinner(p1, p2)
  //
  //    val players = List(Player("Sue", 7),
  //      Player("Bob", 8),
  //      Player("Joe", 4))
  //
  //    val p = players.reduceLeft(winner)
  //    printWinner(p)
  //  }
}


object MyModule {
  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatResult(msg: String, x: Int, f: Int => Int) = {
    val msg = s"The msg of %d is %d"
    msg.format(x, f(x))
    //    s"The absolute value of $x is ${abs(x)}"
  }

  def factorial(n: Int): Int = {

    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 1)
        acc
      else
        go(n - 1, n * acc)
    }
    go(n, 1)
    //    if (n <= 1)
    //      1
    //    else
    //      n * factorial(n - 1)
  }

  def fibonacci(n: Int): Int = {
    //    if (n <= 1)
    //      n
    //    else
    //      fibonacci(n - 1) + fibonacci(n - 2)
    def loop(n: Int, pre: Int, cur: Int): Int = {
      if (n == 0) cur
      else
        loop(n - 1, cur, pre + cur)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    if (as.length <= 1)
      true
    else
      gt(as(1), as(0)) && isSorted(as.tail, gt)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) =>
      (b: B) =>
        f(a, b)
  }

  def unCurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def andThen[A, B, C](f: A => B, g: B => C): A => C = {
    (a: A) => g(f(a))
  }

  def add(a: Int, b: Int) = a + b

  def main(args: Array[String]): Unit = {
    println(formatResult("abs value", -42, abs))
    println(formatResult("factorial value", 7, factorial))
    println(formatResult("fibonacci value", 7, fibonacci))

    println(isSorted(Array(3, 2), (a: Int, b: Int) => a > b))

    println(partial1(1, add)(2))

    println(curry(add)(1)(3))
    println(unCurry(curry(add))(1, 4))

    println(compose(abs, curry(add)(1))(-4))

    val f = (x: Double) => math.Pi / 2 - x
    println(andThen(f, math.sin)(1))
  }
}