package org.hablapps.geofence

import org.hablapps.coalgebraz._
import Coalgebraz._, EntityOps._

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

  // Publishers
  val geoentities: Entity[
      IndexIn[GeoentityIn, Geoentity, String],
      IndexOut[GeoentityOut, Geoentity, String],
      List[(String, Geoentity)],
      List[Geoentity]] =
    geoentity.index(_.id)

  // Subscribers
  val geofences: Entity[
      IndexIn[GeofenceIn, Geofence, String],
      IndexOut[GeofenceOut, Geofence, String],
      List[(String, Geofence)],
      List[Geofence]] =
    geofence.index(_.id)

  // val monitor: Entity[
  //     (String, GeoentityIn),
  //     List[(String, GeofenceOut)],
  //     (List[(String, Geoentity)], List[(String, Geofence)]),
  //     (List[Geoentity], List[Geofence])] =
  //    // TODO: needs a subscription pattern!
  //   geoentity.index(_.id) |>?>| geofence.index(_.id)

  // val clock: Entity[Unit, ClockOut, Int, Int] =
  //   raw(identity, x => _ => (List(Tick(x + 1)), Option(x + 1)))
}
