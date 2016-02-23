package org.hablapps.geofence.state

trait Coverable[A] {
  def covers(a: A)(pos: (Int, Int)): Boolean
}

class CoverableOps[A](val self: A)(implicit C: Coverable[A]) {
  def covers(pos: (Int, Int)): Boolean = C.covers(self)(pos)
}

trait ToCoverableOps {
  implicit def toCoverableOps[A](a: A)(implicit C: Coverable[A]) =
    new CoverableOps[A](a)(C)
}
