package org.hablapps.geofence

import scala.language.implicitConversions

import org.hablapps.coalgebraz._

object Routing {
  implicit def routeInGeofences(
    b: List[(String, Geofence)])(
    i: IndexOut[GeoentityOut, Geoentity, String]): List[IndexIn[GeofenceIn, Geofence, String]] = ???
}
