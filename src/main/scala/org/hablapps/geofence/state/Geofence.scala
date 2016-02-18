package org.hablapps.geofence.state

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
