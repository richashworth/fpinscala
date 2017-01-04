package fpinscala.laziness

import org.scalatest.FunSuite

/**
  * Created by Rich on 03/01/2017.
  */
class StreamTest extends FunSuite {

  val stream = Stream(1, 2, 3, 4)

  test("testToList") {
    assert(List(1, 2, 3, 4) === stream.toList)
  }

  test("testToList for empty stream") {
    assert(List() === Stream.empty.toList)
  }

  test("testTake") {
    assert(List(1, 2) === stream.take(2).toList)
  }

  test("testTakeUsingUnfold") {
    assert(List(1, 2) === stream.takeUsingUnfold(2).toList)
  }

  test("testTake 1 elem") {
    assert(List(1) === stream.take(1).toList)
  }

  test("testTake empty stream") {
    assert(List() === Stream().take(1).toList)
  }

  test("testDrop") {
    assert(List(3, 4) === stream.drop(2).toList)
  }

  test("testDrop empty list") {
    assert(List() === Stream().drop(2).toList)
  }

  test("testDrop too many elems") {
    assert(List() === stream.drop(100).toList)
  }

  test("testTakeWhile") {
    assert(List(1, 2, 3) === stream.takeWhile(_ < 4).toList)
  }

  test("testTakeWhileUsingFold") {
    assert(List(1, 2, 3) === stream.takeWhileUsingUnfold(_ < 4).toList)
  }

  test("testTakeWhile empty stream") {
    assert(List() === Stream[Int]().takeWhile(_ < 4).toList)
  }

  test("testForAll +ve") {
    assert(Stream(2, 4, 8).forAll(_ % 2 == 0))
  }

  test("testForAll -ve") {
    assert(!Stream(2, 4, 7).forAll(_ % 2 == 0))
  }

  test("testHeadOption empty stream") {
    assert(None === Stream().headOption)
  }

  test("testHeadOption") {
    assert(Some(1) === stream.headOption)
  }

  test("testMap") {
    assert(List(2, 3, 4, 5) === stream.map(_ + 1).toList)
  }

  test("testMap empty stream") {
    assert(Empty === Empty.map((x) => x))
  }

  test("testFoldRight") {
    assert(10 === stream.foldRight(0)(_ + _))
  }

  test("testFilter") {
    assert(List(2, 4) === stream.filter(_ % 2 == 0).toList)
  }

  test("testFlatMap") {
    assert(
      List(1, 1, 2, 2, 3, 3, 4,
        4) === stream.flatMap(x => Stream(x, x)).toList)
  }

  test("testConstant") {
    assert(
      List("richard", "richard") === Stream.constant("richard").take(2).toList)
  }

  test("testFrom") {
    assert(List(1, 2, 3, 4) === Stream.from(1).take(4).toList)
  }

  test("testStartsWith") {}

}
