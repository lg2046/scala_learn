package cookbook.fp

/**
 * Created by liguang on 15/2/12.
 * function
 */
class Fun {
  def main(args: Array[String]) {
    val x = (a: Int) => a + 1
    println(x(10))
  }
}
