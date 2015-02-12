package scala_fp.learn_haskell

/**
 * Created by liguang on 15/1/23.
 * x
 */
object One {

  trait Shape {
    def surface = this match {
      case Circle(_, r) => math.Pi * r * r
      case Rectangle(Point(x1, y1), Point(x2, y2)) => math.abs(x1 - x2) * math.abs(y1 - y2)
    }
  }

  case class Point(x: Double, y: Double)

  case class Circle(x: Point, z: Double) extends Shape

  case class Rectangle(x: Point, z: Point) extends Shape




  def main(args: Array[String]) {
    val all = for {x <- 1 to 10; y <- 1 to x; z <- 1 to y
                   if x + y + z == 24 && x * x == y * y + z * z
    } yield (x, y, z)
    println(all.toList)

    println(Rectangle(Point(0, 0), Point(100, 100)).surface)
    println(Circle(Point(0, 0), 24).surface)

    List(1.0,2.0,3.0,4.0).map(Point(1.0, _:Double))
  }

  def factorial(n: Int): BigInt = {
    def go(n: Int, acc: BigInt): BigInt = {
      if (n == 1) acc
      else {
        go(n - 1, acc * n)
      }
    }
    go(n, 1)
  }
}
