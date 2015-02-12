package scala_fp.monoid

/**
 * Created by liguang on 15/1/13.
 */
//Monoid：
//  一种type A   类似很多整数
//  A两个之间的操作产生新的A type 类似加法
//  有一个单元A 类似 加法的0

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}


object On {
  def main(args: Array[String]) {
    val stringMonoid = new Monoid[String] {
      def op(a1: String, a2: String) = a1 + a2

      def zero = ""
    }
  }
}
