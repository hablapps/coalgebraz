package org.hablapps.coalgebraz

trait To[A, B] {
  val to: A => B
}

object To extends ToLowPriorityImplicits0 {

  def apply[A, B](to2: A => B): To[A, B] = new To[A, B] { val to = to2 }

  implicit def isoTo[A, B](implicit ev: A <-> B): To[A, B] = apply(ev.to)

  implicit def isoFrom[A, B](implicit ev: A <-> B): To[B, A] = apply(ev.from)
}

trait ToLowPriorityImplicits0 {
  implicit def eqTo[A]: To[A, A] = To(identity)
}
