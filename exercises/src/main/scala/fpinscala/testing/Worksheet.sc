import fpinscala.state._
import fpinscala.state.RNG
import fpinscala.testing._

val g = Gen.boolean
val p = Gen.listOfN(4,g)

val rng = Simple(4)

