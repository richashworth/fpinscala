import sbt._
import Keys._

object FPInScalaBuild extends Build {
  val opts = Project.defaultSettings ++ Seq(
    scalaVersion := "2.11.7",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      "org.pegdown" % "pegdown" % "1.6.0" % "test"
    )
  )

  lazy val root =
    Project(id = "fpinscala",
      base = file("."),
      settings = opts ++ Seq(
        onLoadMessage ~= (_ + nio2check())
      )) aggregate(chapterCode, exercises, answers)
  lazy val chapterCode =
    Project(id = "chapter-code",
      base = file("chaptercode"),
      settings = opts)
  lazy val exercises =
    Project(id = "exercises",
      base = file("exercises"),
      settings = opts ++ (testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaTest, "-o"), Tests.Argument(TestFrameworks.ScalaTest, "-h", "exercises/target/test-reports")))
    )
  lazy val answers =
    Project(id = "answers",
      base = file("answers"),
      settings = opts)

  def nio2check(): String = {
    val cls = "java.nio.channels.AsynchronousFileChannel"
    try {
      Class.forName(cls); ""
    }
    catch {
      case _: ClassNotFoundException =>
        ("\nWARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.\n" +
          "You are probably running Java < 1.7; answers will not compile.\n" +
          "You seem to be running " + System.getProperty("java.version") + ".\n" +
          "Try `project exercises' before compile, or upgrading your JDK.")
    }
  }
}
