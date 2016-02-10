package org.hablapps.geofenze

import org.hablapps.coalgebraz._
import Coalgebraz._, EntityOps._

object Geomonitor {

  // TODO: ¿se ha movido realmente?
  val geoentity: Entity[GeoentityIn, GeoentityOut, Geoentity, Geoentity] =
    raw(identity, x => _ match {
      case Move(pos2) => (List(Moved(pos2)), Option(x.copy(pos = pos2)))
      case Halt => (List(Halted), None)
    })

  val geofence: Entity[GeofenceIn, GeofenceOut, Geofence, Geofence] =
    raw(identity, x => _ match {
      case Join(id) =>
        (List(Joined(id)), Option(x.copy(elements = x.elements + id)))
      case Leave(id) =>
        (List(Left(id)), Option(x.copy(elements = x.elements - id)))
    })

  val monitor: Entity[
      (String, GeoentityIn),
      List[(String, GeofenceOut)],
      (List[(String, Geoentity)], List[(String, Geofence)]),
      (List[Geoentity], List[Geofence])] =
     // TODO: needs a subscription pattern!
    geoentity.index(_.id) |>?>| geofence.index(_.id)

  // val clock: Entity[Unit, ClockOut, Int, Int] =
  //   raw(identity, x => _ => (List(Tick(x + 1)), Option(x + 1)))
}
