package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Coalgebraz._, dsl._, EntityOps._

import state._

object Geomonitor extends state.State with Routing {

  def geoentity[B, X : Observable[B, ?] : Positionable]
      : Entity[GeolocationIn, GeolocationOut, B, X] =
    entity(_.observe, implicit x => {
      case Move(p) if (x.position != p) => (x.position = p) ~> Moved(p)
      case Halt => halt ~> Halted
      case _ => skip
    })

  def geofence[B, X : Observable[B, ?] : Tickable : Joinable]
      : Entity[ClockOut \/ GeofenceIn, GeofenceOut, B, X] =
    entity(_.observe, implicit x => {
      case -\/(Tick) => x.tick
      case \/-(Join(id)) => x.join(id) ~> Joined(id)
      case \/-(Leave(id)) => x.find(id).fold(skip[GeofenceOut, X]) { t =>
        x.leave(id) ~> Left(id, t._2)
      }
    })

  def timer[B, X : Observable[B, ?] : Tickable]: Entity[Unit, ClockOut, B, X] =
    entity(_.observe, implicit x => _ => x.tick ~> Tick)

  // Entity[
  //   IndexIn[GeoentityIn, Geoentity, String],
  //   IndexOut[GeoentityOut, Geoentity, String],
  //   Map[String, Geoentity],
  //   List[Geoentity]]
  // val geoentities = geoentity.index(_.id, identity)

  // Entity[
  //   Unit \/ IndexIn[GeoentityIn, Geoentity, String],
  //   ClockOut \/ IndexOut[GeoentityOut, Geoentity, String],
  //   (Int, Map[String, Geoentity]),
  //   (Int, List[Geoentity])]
  // val timerAndGeoentities = timer |*| geoentities

  // Entity[
  //   IndexIn[GeofenceIn, Geofence, String],
  //   IndexOut[GeofenceOut, Geofence, String],
  //   Map[String, Geofence],
  //   List[Geofence]]
  // val geofences = geofence.index(_.id, identity)

  // FIXME: It's required to invoke `in` to force `geofences` to adapt its
  // input. Can this be avoided?
  //
  // Entity[
  //   IndexIn[GeoentityIn, Geoentity, String],
  //   IndexOut[GeofenceOut, Geofence, String],
  //   (Map[String, Geoentity], Map[String, Geofence]),
  //   (List[Geoentity], List[Geofence])]
  // val monitor = timerAndGeoentities |>| geofences.in
}
