package org.hablapps.geofence.state

trait Identifiable[K, A] {
  def id(a: A): K
}

class IdentifiableOps[K, A](val a: A)(implicit I: Identifiable[K, A]) {
  def id: K = I.id(a)
}

trait ToIdentifiableOps {
  implicit def toIdentifiableOps[K, A](a: A)(implicit I: Identifiable[K, A]) =
    new IdentifiableOps[K, A](a)(I)
}
