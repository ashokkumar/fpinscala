package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](input: Tree[A]): Long = {
    def loop[A](tree: Tree[A], acc: Long) = {
      input match {
        case Leaf(_) => acc + 1
        case Branch(left, right) => size(left) + size(right)
      }
    }

    loop(input, 0)
  }


}