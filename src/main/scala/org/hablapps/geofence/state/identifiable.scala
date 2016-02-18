package org.hablapps.geofence.state

trait Identifiable[A] {
  def id(a: A): String
}

class IdentifiableOps[A](val a: A)(implicit I: Identifiable[A]) {
  def id: String = I.id(a)
}

trait ToIdentifiableOps {
  implicit def toIdentifiableOps[A](a: A)(implicit I: Identifiable[A]) =
    new IdentifiableOps[A](a)(I)
}
