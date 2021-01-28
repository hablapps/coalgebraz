version := "0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps")

unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "src" / "main" / "scala" / "coalgebraz" / "example" / "geofence-spark-streaming" / "config") }

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.4",
  "org.apache.spark" %% "spark-core" % "1.4.1",
  "org.apache.spark" %% "spark-streaming" % "1.4.1",
  "com.typesafe.akka" %% "akka-actor" % "2.4.4",
  "com.typesafe.akka" %% "akka-stream" % "2.4.4")

initialCommands in console := """
  | import scalaz._, Scalaz._
  | import coalgebraz._, Coalgebraz._
  | import coalgebraz.test._, PropFramework._
  | import coalgebraz.example.candycrush._, CandyCrush._
  | import scala.util.Random
  | import coalgebraz.example.geofence._, Geomonitor._
  |""".stripMargin
