package org.hablapps.geofence.state

trait Tickable[A] {
  def tick(a: A): A
}

object Tickable {
  implicit val longTickable = new Tickable[Long] {
    def tick(a: Long) = a + 1
  }
}

class TickableOps[A](val a: A)(implicit T: Tickable[A]) {
  def tick: A = T.tick(a)
}

trait ToTickableOps {
  implicit def toTickableOps[A](a: A)(implicit P: Tickable[A]) =
    new TickableOps[A](a)(P)
}
