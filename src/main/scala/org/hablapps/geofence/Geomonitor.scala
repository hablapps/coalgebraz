package org.hablapps.geofence

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

  val geofence: Entity[GeofenceIn, GeofenceOut, Geofence, Geofence] =
    raw(identity, x => _ match {
      case Join(id) =>
        (List(Joined(id)), Option(x.copy(elements = x.elements + id)))
      case Leave(id) =>
        (List(Left(id)), Option(x.copy(elements = x.elements - id)))
    })

  val geoentities: Entity[
      IndexIn[GeoentityIn, Geoentity, String],
      IndexOut[GeoentityOut, Geoentity, String],
      List[(String, Geoentity)],
      List[Geoentity]] =
    geoentity.index(_.id)

  val geofences: Entity[
      IndexIn[GeofenceIn, Geofence, String],
      IndexOut[GeofenceOut, Geofence, String],
      List[(String, Geofence)],
      List[Geofence]] =
    geofence.index(_.id)

  val monitor: Entity[
      IndexIn[GeoentityIn, Geoentity, String],
      IndexOut[GeofenceOut, Geofence, String],
      (List[(String, Geoentity)], List[(String, Geofence)]),
      (List[Geoentity], List[Geofence])] =
    geoentities |>|
      geofences.in[IndexOut[GeoentityOut, Geoentity, String]]

  // val clock: Entity[Unit, ClockOut, Int, Int] =
  //   raw(identity, x => _ => (List(Tick(x + 1)), Option(x + 1)))
}
