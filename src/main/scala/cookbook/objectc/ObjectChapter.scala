package cookbook.objectc


/**
 * Created by liguang on 15/2/11.
 * object chapter
 */
object ObjectChapter {
  def main(args: Array[String]) {
    import java.util.{Random => _, _}

    print(Color.Blue)
  }
}

class Foo {
  private val secret = 2
}

object Foo {
  // access the private class field 'secret'
  def double(foo: Foo) = foo.secret * 2
}


