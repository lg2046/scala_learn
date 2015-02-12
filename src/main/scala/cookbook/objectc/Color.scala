package cookbook.objectc

case class Color private(s: String)

object Color {
  val Red = Color("red")
  val Yellow = Color("Yellow")
  val Blue = Color("Blue")
}


//trait Color
//
//case object Red extends Color
//
//case object Yellow extends Color
//
//case object Blue extends Color