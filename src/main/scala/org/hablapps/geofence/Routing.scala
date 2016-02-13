package org.hablapps.geofence

import scala.language.implicitConversions

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Routing {

  implicit val routeInGeofences: Router[
      Map[String, Geofence],
      ClockOut \/ IndexOut[GeoentityOut, Geoentity, String],
      IndexIn[ClockOut \/ GeofenceIn, Geofence, String]] = obs => {
    case -\/(Tick) => obs.toList.map { t =>
      WrapIn[ClockOut \/ GeofenceIn, Geofence, String]((t._1, Tick.left))
    }
    case \/-(WrapOut((n, out))) => out match {
      case Moved(pos) => {
        def f(cn: Boolean, cv: Boolean, e: String => ClockOut \/ GeofenceIn) = {
          obs.toList.filter { t =>
            (t._2.elements contains n) == cn && t._2.covers(pos) == cv
          }.map { t =>
            WrapIn[ClockOut \/ GeofenceIn, Geofence, String]((t._1, e(n)))
          }
        }
        f(true, false, n => \/-(Leave(n))) ++
          f(false, true, n => Join(n).right)
      }
      case Halted => obs.toList.filter(_._2.elements contains n).map { t =>
        WrapIn[ClockOut \/ GeofenceIn, Geofence, String]((t._1, Leave(n).right))
      }
    }
    case _ => List.empty
  }
}
