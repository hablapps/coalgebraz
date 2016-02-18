package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Driver.runIO

import org.hablapps.geofence._
import Geomonitor._, state._

object IO extends App {

  def printMonitor(
      obs: ((Int, Map[String, Geolocation]), Map[String, Geofence])): Unit = {
    val ((ticks, entities), fences) = obs
    println("GEOENTITIES")
    println("-----------")
    entities.foreach(e => println(s"> $e"))
    println("\nGEOFENCES")
    println("---------")
    fences.foreach(f => println(s"> $f"))
    println(s"\nTICKS: $ticks")
    println
  }

  def printOutput(out: List[IndexOut[GeofenceOut, Geofence, String]]): Unit = {
    if (out.nonEmpty) {
      println
      out.foreach(_ match {
        case WrapOut((id1, Left(id2, after))) =>
          println(s"=> '$id2' is leaving '$id1' after '$after' ticks")
        case WrapOut((id1, Joined(id2))) =>
          println(s"=> '$id2' is entering '$id1'")
        case _ => ()
      })
      println
    }
  }

  implicit val readMonitorIn =
    new Read[Unit \/ IndexIn[GeolocationIn, Geolocation, String]] {
      def read(s: String) = {
        s.split(" ") match {
          case Array("Tick") => Option(-\/(()))
          case Array("Geolocation", id) =>
            Option(\/-(Attach(Geolocation(id, (1, 1)))))
          case Array("Halt", id) => Option(\/-(WrapIn((id, Halt))))
          case Array("Move", id, a, b) => {
            (Read[Int].read(a) |@| Read[Int].read(b)) { (x, y) =>
              \/-(WrapIn((id, Move((x, y)))))
            }
          }
          case _ => None
        }
      }
    }

  /* Move around! */

  println("""
    |   ________              _____
    |  /  _____/  ____  _____/ ____\____   ____   ____  ____
    | /   \  ____/ __ \/  _ \   __\/ __ \ /    \_/ ___\/ __ \
    | \    \_\  \  ___(  <_> )  | \  ___/|   |  \  \__\  ___/
    |  \______  /\___  >____/|__|  \___  >___|  /\___  >___  >
    |         \/     \/                \/     \/     \/    \/
    |""".stripMargin)

  runIO(monitor)(
    ((0, List(Geolocation("jesus", (4, 4)))),
     List(
       Geofence("guadarrama", (2, 5), 0),
       Geofence("mostoles", (3, 4), 1, Set(("jesus", 0))),
       Geofence("leganes", (9, 3), 1),
       Geofence("alcorcon", (6, 2), 1))),
     printMonitor,
     printOutput)
}
