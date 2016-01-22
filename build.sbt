name := "coalgebraz"

version := "0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-core" % "7.2.0")

initialCommands in console := """
  | import scalaz._, Scalaz._
  | import org.hablapps.coalgebraz._
  | import Driver._
  | import Coalgebraz._
  | import org.hablapps.candy._
  | import Cocandy._
  | import scala.util.Random
  |""".stripMargin
