package scala_fp.functional_data_structure

import scala.util.control.Breaks._

/**
 * Created by liguang on 15/2/9.
 * c
 */
case class Address(stress: String)

class Person7(private var _name: String, private val _age: Int) {

  var score: Int = _
  var address = None: Option[Address]
  lazy val a = {
    println("fuck")
    1
  }

  def this(name: String = "s") {
    this(name, 10)
  }

  def name = _name

  def name_$eq(s: String) = {
    _name = s
  }


  def age = _age
}

class NewPerson(n: String, a: Int, val color: String = "Gray") extends Person7(n, a)

object Person7 {
  def apply() = {
    new Person7("name")
  }
}


abstract class Pet(name: String) {
  val greeting: String
  var age: Int

  def sayHello = 1

  override def toString = s"I say $greeting, and I'm $age"
}

object NewtonsMethod {
  def main(args: Array[String]) {
    val p = new NewPerson("a", 10)
    println(p.age)
    println(p.color)

    val p2 = new NewPerson("a", 10, "black")
    println(p2.color)

    println(p2.age)
    //    driver()
    //    val p1 = Person7()
    //    val p2 = new Person7("aa")
    //    val p3 = new Person7()
    //    p3.name = "age"
    //    p3.name_$eq("laiba")
    //    println(p1.age)
    //    println(p2.age)
    //    println(p3.name)
    //    p3.a
    //    p3.score = 10
    //    print(p3.score)
  }

  //  val text = io.Source.fromFile("/etc/passwd").getLines.foreach(println)

  def driver() = {

    val fx = (x: Double) => 3 * x + math.sin(x) - math.pow(math.E, x)
    val dfx = (x: Double) => 3 + math.cos(x) - math.pow(math.E, x)

    val initialGuess = 0.0
    val tolerance = 0.000001

    val answer = newtonsMethod(fx, dfx, initialGuess, tolerance)
    println("answer:")
    println(answer)
  }

  @throws(classOf[Exception])
  def newtonsMethod(fx: Double => Double,
                    dfx: Double => Double,
                    init: Double,
                    tolerance: Double): Double = {

    var x1 = init

    breakable {
      while (true) {
        val xNext = x1 - fx(x1) / dfx(x1)
        if (math.abs(xNext - x1) < tolerance) break()

        x1 = xNext
        println(xNext)
      }
    }

    x1
  }


}
