import fpinscala.state.RNG
import fpinscala.state.RNG.Simple

val x = Simple(4)

val (res, nextState) = x.nextInt

RNG.nonNegativeEven(x)

