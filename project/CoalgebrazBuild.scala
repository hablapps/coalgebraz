import sbt._
import Keys._

object CoalgebrazBuild extends Build {

    lazy val root = Project(id = "coalgebraz",
                            base = file(".")) aggregate(core, example)

    lazy val core = Project(id = "core",
                            base = file("core"))

    lazy val example = Project(id = "example",
                               base = file("example")) dependsOn(core)
}
