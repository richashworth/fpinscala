package fpinscala.state

import State._

object WaterFSM {

  sealed trait WaterState
  case object Ice extends WaterState
  case object LiquidWater extends WaterState
  case object WaterVapour extends WaterState

  sealed trait Command
  case object Heat extends Command
  case object Cool extends Command
  case object Sublime extends Command

  def interpret: Command => WaterState => WaterState =
    c =>
      w =>
        (c, w) match {
          case (Heat, Ice) => LiquidWater
          case (Heat, LiquidWater) => WaterVapour
          case (Cool, WaterVapour) => LiquidWater
          case (Cool, LiquidWater) => Ice
          case (Sublime, Ice) => WaterVapour
          case _ => w
    }

  def sequenceCommands(inputs: List[Command]): State[WaterState, List[Unit]] =
    sequence(inputs.map((cmd) => modify(interpret(cmd))))

}
