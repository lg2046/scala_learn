package scala_fp.functional_data_structure


trait TrafficLight

object Red extends TrafficLight

object Yellow extends TrafficLight

object Green extends TrafficLight


trait Eq[-A] {
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
    println(testEq(2, 1))
    println(testEq(1, 1))
    println(implicitly[Eq[TrafficLight]].===(Red, Red))
  }

  def testEq[A: Eq](a: A, b: A): Boolean = implicitly[Eq[A]].===(a, b)
}
