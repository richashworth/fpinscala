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

  test("init should return all but the last element of a list") {
    val list = List(4, 2, 3000, 3, 4, 5, 6, 7)
    assert(List.init(list) === List(4, 2, 3000, 3, 4, 5, 6))
  }

  test("length should return zero for an empty list") {
    assert(List.length(Nil) === 0)
  }

  test("length should return the number of elements in a list") {
    val list = List(4, 2, 3000, 3, 4, 5, 6, 7)
    assert(List.length(list) === 8)
  }

  test("foldLeft should help compute the sum of a list") {
    val list = List(1, 2, 3, 4)
    val sum = List.foldLeft(list, 0)((x, acc) => acc + x)
    assert(10 === sum)
  }

  test("foldLeft should help compute the product of a list") {
    val list = List(1, 2, 3, 4)
    val product = List.foldLeft(list, 1)((acc, x) => acc * x)
    assert(24 === product)
  }

  test("foldLeft should help compute the length of a list") {
    val list = List(1, 2, 3, 4)
    val length = List.foldLeft(list, 0)((acc, _) => acc + 1)
    assert(4 === length)
  }

  test("the reverse of an empty list is an empty list") {
    val reverse = List.reverse(Nil)
    assert(Nil === reverse)
  }

  test("reverse should return the reverse of a list") {
    val list = List(1, 2, 3, 4)
    val reverse = List.reverse(list)
    assert(reverse === List(4, 3, 2, 1))
  }

  test("map should apply a function f to each element") {
    val list = List(1, 2, 3, 4)
    val mapped = List.map(list)(_ + 1)
    assert(List(2, 3, 4, 5) == mapped)
  }

  test("map should handle the empty list") {
    assert(Nil === (List.map(List[Int]())(_ + 1)))
  }

}
