case class Employee(
  val name: String,
  val boss: Option[Employee]
)

val helen = Employee("Helen", None)
val mike = Employee("Mike", Some(helen))
val rich = Employee("Rich", Some(mike))


helen.boss.flatMap(_.boss)

val x = rich.boss.flatMap(_.boss.map(_.name))

val y = for {
  a <- rich.boss
  b <- a.boss
} yield b.name

x == y
