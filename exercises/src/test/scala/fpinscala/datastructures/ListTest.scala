package fpinscala.datastructures

/**
  * Created by Rich on 20/12/2016.
  */
class ListTest extends org.scalatest.FunSuite {

  test("tail should remove the first element of a list") {
    val list = List(1, 2, 3, 4, 5)
    val tail = List.tail(list)
    assert(tail === List(2, 3, 4, 5))
  }

  test("tail should throw an exception when called on the empty list") {
    intercept[IllegalArgumentException] {
      List.tail(Nil)
    }
  }

  test("setHead should replace the first element of a list") {
    val list = List(1, 2, 3, 4, 5)
    assert(List.setHead(list, 0) === List(0, 2, 3, 4, 5))
  }

  test("setHead should deal with empty lists") {
    assert(List.setHead(Nil, "a") === List("a"))
  }

  test("drop should remove first n elems from list") {
    val list = List(1, 2, 3, 4, 5)
    assert(List.drop(list, 3) === List(4, 5))
  }

  test("dropWhile should remove elems until x s.t. f(x) is false") {
    val f = (x: Int) => x % 2 == 0
    val list = List(4, 2, 3000, 3, 4, 5, 6, 7)
    assert(List.dropWhile(list, f) === List(3, 4, 5, 6, 7))
  }

}
