package scala_fp.chapter5_lazy

/**
 * Created by liguang on 15/1/13.
 */
object One {

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
    onTrue
    onFalse
    onFalse
  }

  def main(args: Array[String]) {
    println(if2(1 == 2, {
      println("true")
      1 + 1
    }, {
      println("false")
      2 + 2
    }))

    def pair2(i: => Int) = {
      lazy val j = i
      (j, j)
    }
    println(pair2({ println("hi"); 1 }))
    //    println(if2(cond = false, sys.error("fail"), 3))
  }

}
