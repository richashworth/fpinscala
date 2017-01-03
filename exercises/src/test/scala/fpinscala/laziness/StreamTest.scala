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
  }

  test("testForAll") {

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
