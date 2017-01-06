package fpinscala.state

import annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG { // NB - this was called SimpleRNG in the book text

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

  def unit[A](a: A): Rand[A] = // think of this as the 'constructor' for the Rand[A] type
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleUsingMap: Rand[Double] =
    map(nonNegativeInt)(i => ("0." + i).toDouble)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (res, nextState) = rng.nextInt
    if (res < 0) (-(res + 1), nextState)
    else (res, nextState)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (res, nextState) = nonNegativeInt(rng)
    val strDble = "0." + res
    (strDble.toDouble, nextState)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intResult, nextState) = rng.nextInt
    val (doubleResult, _) = double(rng)
    ((intResult, doubleResult), nextState)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intResult, doubleResult), nextState) = intDouble(rng)
    ((doubleResult, intResult), nextState)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng2) = double(rng)
    val (double2, rng3) = double(rng2)
    val (double3, rng4) = double(rng3)
    ((double1, double2, double3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int, acc: List[Int], state: RNG): (List[Int], RNG) = {
      val (intResult, nextState) = state.nextInt
      if (c == 0) (acc, nextState)
      else {
        go(c - 1, intResult :: acc, nextState)
      }
    }
    go(count, List(), rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)((x) =>
      if (x < n) unit(x) else nonNegativeLessThan(n))
  }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
