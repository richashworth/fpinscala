package fpinscala.state

import org.scalatest.FunSuite
import State._

/**
 * Created by Rich on 28/12/2016.
 */
class StateTest extends FunSuite {

  test("Vending machine FSM can process input list") {

    val initialState = Machine(true, 5, 10)

    // Attempt to buy 4 candies
    val inputCommands = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

    val program = VendingMachineInterpreter.simulateMachine(inputCommands)

    val result = program.run(initialState)

    assert(result === ((1,14), Machine(true, 1, 14)))
  }

}
