package fpinscala.testing

import fpinscala.state._
import fpinscala.state.RNG
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file
is just the shell, which you can fill in and modify while working through the
chapter.
 */

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(this.listOfN(_))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen[Int](
      State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
}
