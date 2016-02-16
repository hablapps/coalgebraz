package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Coalgebraz._, dsl._, EntityOps._

import Routing._

object Geomonitor {

  val geoentity: Entity[GeoentityIn, GeoentityOut, Geoentity, Geoentity] =
    next(implicit x => {
      case Move(pos2) if (x.pos != pos2) => x.copy(pos = pos2) ~> Moved(pos2)
      case Halt => halt ~> Halted
      case _ => skip
    })

  val geofence: Entity[ClockOut \/ GeofenceIn, GeofenceOut, Geofence, Geofence] =
    next(implicit x => {
      case -\/(Tick) => x.copy(relation = x.relation.map(_.map(_ + 1)))
      case \/-(Join(id)) =>
        x.copy(relation = x.relation + (id -> 0)) ~> Joined(id)
      case \/-(Leave(id)) => {
        x.relation.find(_._1 == id).fold(skip[GeofenceOut, Geofence]) { t =>
          x.copy(relation = x.relation - t) ~> Left(id, t._2)
        }
      }
    })

  val clock: Entity[Unit, ClockOut, Int, Int] =
    next(implicit x => _ => (x + 1) ~> Tick)

  // Entity[
  //   IndexIn[GeoentityIn, Geoentity, String],
  //   IndexOut[GeoentityOut, Geoentity, String],
  //   Map[String, Geoentity],
  //   List[Geoentity]]
  val geoentities = geoentity.index(_.id)

  // Entity[
  //   Unit \/ IndexIn[GeoentityIn, Geoentity, String],
  //   ClockOut \/ IndexOut[GeoentityOut, Geoentity, String],
  //   (Int, Map[String, Geoentity]),
  //   (Int, List[Geoentity])]
  val clockAndGeoentities = clock |*| geoentities

  // Entity[
  //   IndexIn[GeofenceIn, Geofence, String],
  //   IndexOut[GeofenceOut, Geofence, String],
  //   Map[String, Geofence],
  //   List[Geofence]]
  val geofences = geofence.index(_.id)

  // FIXME: It's required to invoke `in` to force `geofences` to adapt its
  // input. Can this be avoided?
  //
  // Entity[
  //   IndexIn[GeoentityIn, Geoentity, String],
  //   IndexOut[GeofenceOut, Geofence, String],
  //   (Map[String, Geoentity], Map[String, Geofence]),
  //   (List[Geoentity], List[Geofence])]
  val monitor = clockAndGeoentities |>| geofences.in
}
