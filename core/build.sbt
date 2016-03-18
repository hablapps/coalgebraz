version := "0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps")

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.scalaz" %% "scalaz-core" % "7.2.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.4")

initialCommands in console := """
  | import scalaz._, Scalaz._
  | import coalgebraz._, Coalgebraz._
  | import coalgebraz.test._, PropFramework._
  |""".stripMargin
