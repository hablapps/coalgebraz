package org.hablapps.geofence

import scala.language.implicitConversions

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Routing {
  // TODO: sooo ugly, mainly because of type inference limitations with pattern
  // matching. Is there a canononical way to improve it?
  implicit def routeInGeofences(
    obs: Map[String, Geofence])(
    i: ClockOut \/ IndexOut[GeoentityOut, Geoentity, String]):
      List[IndexIn[ClockOut \/ GeofenceIn, Geofence, String]] = i match {
    case -\/(Tick) => obs.toList.map { t =>
      WrapIn((t._1, -\/(Tick))): IndexIn[ClockOut \/ GeofenceIn, Geofence, String]
    }
    case \/-(WrapOut((n, out))) => out match {
      case Moved(pos) => {
        def f(
            cnt: Boolean,
            cvr: Boolean,
            ev: String => ClockOut \/ GeofenceIn) = {
          obs.filter { t =>
            (t._2.elements contains n) == cnt && t._2.covers(pos) == cvr
          }.map { t =>
            WrapIn((t._1, ev(n))): IndexIn[ClockOut \/ GeofenceIn, Geofence, String]
          }.toList
        }
        f(true, false, n => \/-(Leave(n))) ++ f(false, true, n => \/-(Join(n)))
      }
      case Halted => {
        obs.filter(_._2.elements contains n).map { t =>
          WrapIn((t._1, \/-(Leave(n)))): IndexIn[ClockOut \/ GeofenceIn, Geofence, String]
        }.toList
      }
    }
    case _ => List.empty
  }
}
