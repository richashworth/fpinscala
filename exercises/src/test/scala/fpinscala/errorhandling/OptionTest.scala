package fpinscala.errorhandling

import org.scalatest.FunSuite

/**
  * Created by Rich on 27/12/2016.
  */
class OptionTest extends FunSuite {

  private val f = (x: Int) => x + 1
  private val x = Some(4)

  test("testMap for Some(x)") {
    assert(Some(5) === x.map(f))
  }

  test("testMap for None") {
    assert(None === None.map(f))
  }

  test("testGetOrElse for Some(x)") {
    assert(4 === x.getOrElse(0))
  }

  test("testGetOrElse None") {
    assert(0 === None.getOrElse(0))
  }

  test("testVariance empty seq") {
    assert(None === Option.variance(Nil))
  }

  test("testVariance non-empty seq") {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    assert(Some(2.0) === Option.variance(seq))
  }

  test("map2 should return a Some() if neither values are None") {
    assert(
      Some(42) === Option.map2(Some(20), Some(22))((x: Int, y: Int) => x + y))
  }

  test("map2 should return None if the first value is None") {
    assert(None === Option.map2(None, Some(0))((x: Int, y) => x + y))
  }

  test("map2 should return None if the second value is None") {
    assert(None === Option.map2(None, Some(0))((x: Int, y) => x + y))
  }

  test("sequence should return None if any input Options are None") {
    assert(None === Option.sequence(List(Some(0), Some(1), None, Some(2))))
  }

  test("sequence should return Some(List) if no input Options are None") {
    assert(Some(List(0,1,2))=== Option.sequence(List(Some(0), Some(1), Some(2))))
  }

}
