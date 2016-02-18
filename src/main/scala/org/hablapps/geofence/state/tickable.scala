package org.hablapps.geofence.state

trait Tickable[A] {
  def tick(a: A): A
}

object Tickable {
  implicit val intInstance = new Tickable[Int] {
    def tick(a: Int) = a + 1
  }
}

class TickableOps[A](val a: A)(implicit T: Tickable[A]) {
  def tick: A = T.tick(a)
}

trait ToTickableOps {
  implicit def toTickableOps[A](a: A)(implicit P: Tickable[A]) =
    new TickableOps[A](a)(P)
}
