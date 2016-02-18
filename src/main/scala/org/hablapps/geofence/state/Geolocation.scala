package org.hablapps.geofence.state

case class Geolocation(id: String, pos: (Int, Int))

object Geolocation {
  implicit val positionableInstance = new Positionable[Geolocation] {
    def position(a: Geolocation) = a.pos
    def position_=(a: Geolocation)(p: (Int, Int)) = a.copy(pos = p)
  }

  implicit val identifiableInstance = new Identifiable[Geolocation] {
    def id(a: Geolocation): String = a.id
  }
}
