package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(counter: Long, s: Stream[A], acc: Stream[A]): Stream[A] = (counter, s) match {
      case (0, _) => acc
      case (_ , Cons(h, t)) => go(counter - 1, t(), Cons(h, () => acc))
      case (_, _) => acc
    }

    go(n, this, Empty)
  }

  def drop(n: Int): Stream[A] = {
    def go(s: Stream[A], counter: Int): Stream[A] = s match {
      case Cons(_, t) if counter > 0 => go(t(), counter - 1)
      case _ => s
    }
    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

//  def headOption: Option[A] = this match {
//    case Cons(h, _) => Some(h())
//    case _ => None
//  }
  def headOption: Option[A] = foldRight(None[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if(f(a)) cons(a,b) else b)

  def append[B>:A](s: Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), acc :+ h())
      case _ => acc
    }

    go(this, List())
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  def fib: Stream[Long] = {
    def go(first: Long, next: Long): Stream[Long] = cons(first, go(next, first+next))
    go(0,1)
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}

object Sample {
  def main (args: Array[String] ): Unit = {
    println(Stream(1,2,3).drop(2).toList)
  }

}

