package fp

class Person(name: String, age: Int) {
  private[fp] def canEqual(a: Any) = a.isInstanceOf[Person]

  override def equals(that: Any): Boolean = that match {
    case that: Person => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + age
    result = prime * result + (if (name == null) 0 else name.hashCode)
    result
  }
}


import org.scalatest.FunSuite

/**
 * Created by liguang on 15/2/10.
 * test
 */
class PersonTest extends FunSuite {

  val nimoy = new Person("Leonard Nimoy", 82)
  val nimoy2 = new Person("Leonard Nimoy", 82)
  val shatner = new Person("William Shatner", 82)
  val ed = new Person("Ed Chigliak", 20)

  test("fuck you") {
    assert(nimoy == nimoy2)
  }
}
