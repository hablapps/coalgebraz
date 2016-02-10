package org.hablapps.geofenze

import org.hablapps.coalgebraz._

case class Geoentity(id: String, pos: (Int, Int))

sealed trait GeoentityIn
case class Move(pos: (Int, Int)) extends GeoentityIn
case object Halt extends GeoentityIn

sealed trait GeoentityOut
case class Moved(pos: (Int, Int)) extends GeoentityOut
case object Halted extends GeoentityOut

case class Geofence(
    id: String,
    pos: (Int, Int),
    radius: Int,
    elements: Set[String]) {
  def covers(pos2: (Int, Int)): Boolean = {
    def f(a: Int, b: Int) = a <= b + radius && a >= b - radius
    f(pos2._1, pos._1) && f(pos2._2, pos._2)
  }
}

sealed trait GeofenceIn
case class Join(id: String) extends GeofenceIn
case class Leave(id: String) extends GeofenceIn

sealed trait GeofenceOut
case class Joined(id: String) extends GeofenceOut
case class Left(id: String) extends GeofenceOut

sealed trait ClockOut
case class Tick(t: Int) extends ClockOut
