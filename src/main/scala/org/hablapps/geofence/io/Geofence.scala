package org.hablapps.geofence.io

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._, Coalgebraz._

import org.hablapps.geofence.state._

case class Geofence(
    id: String,
    pos: (Int, Int),
    radius: Int,
    relation: Set[(String, Long)] = Set()) {

  def elements = relation.map(_._1)

  def covers(pos2: (Int, Int)): Boolean = {
    def f(a: Int, b: Int) = a <= b + radius && a >= b - radius
    f(pos2._1, pos._1) && f(pos2._2, pos._2)
  }
}

object Geofence {

  implicit val geofenceTickable = new Tickable[Geofence] {
    def tick(a: Geofence) = a.copy(relation = a.relation.map(_.map(_ + 1)))
  }

  implicit val geofenceJoinable = new Joinable[Geofence] {

    def join(a: Geofence)(id: String) =
      a.copy(relation = a.relation + (id -> 0))

    def leave(a: Geofence)(id: String): Geofence = find(a)(id).fold(a) {
      t => a.copy(relation = a.relation - t)
    }

    def find(a: Geofence)(id: String) =
      a.relation.find(_._1 == id)
  }

  implicit val geofenceCoverable = new Coverable[Geofence] {
    def covers(a: Geofence)(pos: (Int, Int)) = a.covers(pos)
  }
}
