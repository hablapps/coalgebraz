package org.hablapps.geofence.state

trait Positionable[A] {
  def position(a: A): (Int, Int)
  def position_=(a: A)(p: (Int, Int)): A
}

class PositionableOps[A](val a: A)(implicit P: Positionable[A]) {
  def position: (Int, Int) = P.position(a)
  def position_=(pos: (Int, Int)): A = P.position_=(a)(pos)
}

trait ToPositionableOps {
  implicit def toPositionableOps[A](a: A)(implicit P: Positionable[A]) =
    new PositionableOps[A](a)(P)
}
