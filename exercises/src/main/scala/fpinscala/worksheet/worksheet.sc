def addThree(a: Int)(b: Int, c: Int) = a + b + c

addThree(1)(2, 3)

val addOneToTwoArgs = addThree(1) _

addOneToTwoArgs(3, 2)

addThree(5)(4,99)


