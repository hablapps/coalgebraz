package org.hablapps.geofence

import org.hablapps.coalgebraz._

case class Geolocation(id: String, pos: (Int, Int))

sealed trait GeolocationIn // geopositionin
case class Move(pos: (Int, Int)) extends GeolocationIn
case object Halt extends GeolocationIn

sealed trait GeolocationOut // ""out
case class Moved(pos: (Int, Int)) extends GeolocationOut
case object Halted extends GeolocationOut

case class Geofence(
    id: String,
    pos: (Int, Int),
    radius: Int,
    relation: Set[(String, Int)] = Set()) {

  def elements = relation.map(_._1)

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
case class Left(id: String, after: Int) extends GeofenceOut

sealed trait ClockOut
case object Tick extends ClockOut
