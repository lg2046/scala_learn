package cookbook.objectc

/**
 * Created by liguang on 15/2/2.
 * ++string++
 */
object String01 {

  implicit class StringUtils(s: String) {
    def increment = {
      s.capitalize
    }
  }

  def main(args: Array[String]) {
    println("google" == new String("a"))
    val s = "eggs, milk, butter, Coco Puffs"
    s.split(",")
    s.split(',')
    print(s"$s")

    val reg = ".*(mil).*".r
    s match {
      case reg(m) => m
      case _ => "None"
    }
    reg.findAllMatchIn(s).toList

    val pattern = "([0-9]+) ([A-Za-z]+)".r

    val pattern(count, fruit) = "100 apples"
    "fuckme".increment
  }
}


