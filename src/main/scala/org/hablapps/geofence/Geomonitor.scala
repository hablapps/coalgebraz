package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Coalgebraz._, EntityOps._

import Routing._

object Geomonitor {

  val geoentity: Entity[GeoentityIn, GeoentityOut, Geoentity, Geoentity] =
    raw(identity, x => _ match {
      case Move(pos2) if x.pos != pos2 =>
        (List(Moved(pos2)), Option(x.copy(pos = pos2)))
      case Halt => (List(Halted), None)
      case _ => (List.empty, Option(x))
    })

  val geofence: Entity[ClockOut \/ GeofenceIn, GeofenceOut, Geofence, Geofence] =
    raw(identity, x => _ match {
      case -\/(Tick) =>
        (List(), Option(x.copy(relation = x.relation.map(_.map(_ + 1)))))
      case \/-(Join(id)) =>
        (List(Joined(id)), Option(x.copy(relation = x.relation + (id -> 0))))
      case \/-(Leave(id)) => {
        x.relation.find(_._1 == id).fold(
          (List.empty[GeofenceOut], Option(x))) { t =>
          (List(Left(id, t._2)), Option(x.copy(relation = x.relation - t)))
        }
      }
    })

  val clock: Entity[Unit, ClockOut, Int, Int] =
    raw(identity, x => _ => (List(Tick), Option(x + 1)))

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
