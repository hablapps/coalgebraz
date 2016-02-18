package org.hablapps.geofence.state

trait State extends ToPositionableOps
    with ToIdentifiableOps
    with ToTickableOps
    with ToJoinableOps {

  sealed trait GeolocationIn
  case class Move(pos: (Int, Int)) extends GeolocationIn
  case object Halt extends GeolocationIn

  sealed trait GeolocationOut
  case class Moved(pos: (Int, Int)) extends GeolocationOut
  case object Halted extends GeolocationOut

  sealed trait GeofenceIn
  case class Join(id: String) extends GeofenceIn
  case class Leave(id: String) extends GeofenceIn

  sealed trait GeofenceOut
  case class Joined(id: String) extends GeofenceOut
  case class Left(id: String, after: Long) extends GeofenceOut

  sealed trait ClockOut
  case object Tick extends ClockOut
}
