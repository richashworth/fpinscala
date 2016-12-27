package fpinscala.errorhandling

import org.scalatest.FunSuite

/**
  * Created by Rich on 27/12/2016.
  */
class OptionTest extends FunSuite {

  private val f = (x: Int) => x + 1

  test("testMap for Some(x)") {
    val x = Some(4)
    assert(Some(5) === x.map(f))
  }

  test("testMap for None") {
    assert(None === None.map(f))
  }

  test("testFlatMap") {
  }

  test("testFilter") {

  }

  test("testGetOrElse") {

  }

  test("testOrElse") {

  }

}
