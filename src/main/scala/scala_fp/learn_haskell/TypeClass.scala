package scala_fp.learn_haskell

trait YesNo[A] {
  def yesno(value: A): Boolean
}

object YesNo {
  implicit val intYesNo = new YesNo[Int] {
    def yesno(value: Int) =
      value match {
        case 0 => false
        case _ => true
      }
  }

  implicit val stringYesNo = new YesNo[String] {
    def yesno(value: String) =
      value match {
        case "" => false
        case _ => true
      }
  }
}

object TestDa2 {
  // 方法声明使用type class约束
  // 指定这里可以访问一个类型为YesNo[A]的对象  这个对象里面定义了需要满足YesNo TypeClass的方法
  def write[A: YesNo](value: A): Boolean = {
    implicitly[YesNo[A]].yesno(value)
  }


  def main(args: Array[String]) {
    println(write(2))
    println(write("a"))
    println(write(""))
  }
}