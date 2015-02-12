package scala_fp.functional_data_structure
import cookbook.objectc.Color
/**
 * Created by liguang on 15-1-4.
 * For test
 */

object Test {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A) extends List[A]

  def main(args: Array[String]) {
    Cons(1)

    print(Color.Red)

  }
}
