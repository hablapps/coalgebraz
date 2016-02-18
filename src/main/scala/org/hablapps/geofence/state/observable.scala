package org.hablapps.geofence.state

// XXX: seems like a candidate to be located in `coalgebraz`
trait Observable[B, A] {
  def observe(a: A): B
}

class ObservableOps[K, A](val a: A)(implicit O: Observable[K, A]) {
  def observe: K = O.observe(a)
}

trait ToObservableOps {
  implicit def toObservableOps[K, A](a: A)(implicit I: Observable[K, A]) =
    new ObservableOps[K, A](a)(I)
}
