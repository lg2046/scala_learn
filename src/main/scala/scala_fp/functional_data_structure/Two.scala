package scala_fp.functional_data_structure

/**
 * Created by liguang on 15/1/9.
 */
object Two {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  object Tree {

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def size[A](t: Tree[A]): Int =
      fold(t)(a => 1)(1 + _ + _)

    //      t match {
    //      case Leaf(_) => 1
    //      case Branch(left, right) => 1 + size(left) + size(right)
    //    }


    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(left, right) => maximum(left) max maximum(right)
    }

    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def main(args: Array[String]) {
    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val leaf3 = Leaf(3)
    val leaf4 = Leaf(4)
    val branch1 = new Branch(leaf1, leaf2)
    val branch2 = new Branch(leaf3, leaf4)
    val tree = new Branch(branch1, branch2)

    println(Tree.size(leaf1))
    println(Tree.size(leaf2))
    println(Tree.size(branch1))
    println(Tree.size(branch2))
    println(Tree.size(tree))

    println(Tree.maximum(tree))
    println(Tree.depth(tree))
    println(Tree.map(tree)(_ + 1))
  }
}
