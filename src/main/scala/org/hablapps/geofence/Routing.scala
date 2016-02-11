package org.hablapps.geofence

import scala.language.implicitConversions

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Routing {
  implicit def routeInGeofences(
    obs: List[(String, Geofence)])(
    i: IndexOut[GeoentityOut, Geoentity, String]):
      List[IndexIn[GeofenceIn, Geofence, String]] = i match {
    case WrapOut((n, out)) => out match {
      case Moved(pos) => {
        def f(cnt: Boolean, cvr: Boolean, ev: String => GeofenceIn) = {
          obs.filter { t =>
            (t._2.elements contains n) == cnt && t._2.covers(pos) == cvr
          }.map { t =>
            WrapIn((t._1, ev(n))): IndexIn[GeofenceIn, Geofence, String]
          }
        }
        f(true, false, Leave.apply) ++ f(false, true, Join.apply)
      }
      case Halted => {
        obs.filter(_._2.elements contains n).map { t =>
          WrapIn((t._1, Leave(n))): IndexIn[GeofenceIn, Geofence, String]
        }
      }
    }
    case _ => List.empty
  }
}
