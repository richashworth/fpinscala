package fpinscala.state

import fpinscala.state.WaterFSM._
import org.scalatest.FunSuite

class WaterFSMTest extends FunSuite {

  test("Water FSM can process input list") {

    val initialState = Ice

    val inputCommands = List(Sublime, Heat, Cool, Heat, Cool, Cool)

    val program: State[WaterState, List[Unit]] =
      WaterFSM.sequenceCommands(inputCommands)

    val (_, result) = program.run(initialState)

    assert(result === Ice)

  }

}
