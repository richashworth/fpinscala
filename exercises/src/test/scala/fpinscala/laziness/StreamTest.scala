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

  test("testTakeWhile empty stream") {
    assert(List() === Stream[Int]().takeWhile(_ < 4).toList)
  }

  test("testForAll +ve") {
    assert(Stream(2, 4, 8).forAll(_ % 2 == 0))
  }

  test("testForAll -ve") {
    assert(!Stream(2, 4, 7).forAll(_ % 2 == 0))
  }


  test("testHeadOption") {

  }

  test("testFoldRight") {

  }

  test("testFind") {

  }

  test("testExists") {

  }

  test("testStartsWith") {

  }

}
