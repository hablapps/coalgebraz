package org.hablapps.geofence.io

import org.hablapps.coalgebraz._, Coalgebraz._

import org.hablapps.geofence.state._

case class Geolocation(id: String, pos: (Int, Int))

object Geolocation {

  implicit val geolocationPositionable = new Positionable[Geolocation] {
    def position(a: Geolocation) = a.pos
    def position_=(a: Geolocation)(p: (Int, Int)) = a.copy(pos = p)
  }

  implicit val geolocationObservable = new Observable[(String, (Int, Int)), Geolocation] {
    def observe(a: Geolocation) = (a.id, a.pos)
  }

  implicit val geolocationIndexable = new Indexable[String, Geolocation] {
    def index(a: Geolocation): String = a.id
  }
}
