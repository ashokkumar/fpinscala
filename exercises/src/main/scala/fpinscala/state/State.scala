package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, newState) = rng.nextInt
    if (value < 0) (-(value +1) , newState) else (value, newState)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value, newState) = nonNegativeInt(rng)
    ((value / Int.MaxValue.toDouble + 1), newState)
  }

  def doubleMap(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(value => (value / Int.MaxValue.toDouble + 1))
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intVal, newRng1) = nonNegativeInt(rng)
    val (doubleVal, newRng2) = double(newRng1)
    ((intVal, doubleVal), newRng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (doubleVal, rng1) = double(rng)
    val (intVal, rng2) = nonNegativeInt(rng1)
    ((doubleVal, intVal), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List(), rng)
    case _ => {
      val (v, r) = rng.nextInt
      val (v1, r1) = ints(count - 1)(r)
      (v :: v1, r1)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =  ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
