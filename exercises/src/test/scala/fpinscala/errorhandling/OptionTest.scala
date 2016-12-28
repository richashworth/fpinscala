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

}
