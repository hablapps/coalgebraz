package org.hablapps.coalgebraz

trait To[A, B] {
  val to: A => B
}

object To extends ToLowPriorityImplicits {

  def apply[A, B](to2: A => B): To[A, B] = new To[A, B] { val to = to2 }

  implicit def tupleSelect[A, B]: To[(A, B), A] = apply(_._1)
}

trait ToLowPriorityImplicits extends ToLowPriorityImplicits2 {
  implicit def toFirst[A, B, C](implicit ev: A -> C) = To[(A, B), (C, B)](
    { case (a, b) => (ev.to(a), b) })
}

trait ToLowPriorityImplicits2 extends ToLowPriorityImplicits1 {
  implicit def eqTo[A]: To[A, A] = To(identity)
}

trait ToLowPriorityImplicits1 extends ToLowPriorityImplicits0 {

  implicit def isoTo[A, B](implicit ev: A <-> B): To[A, B] = To(ev.to)

  implicit def isoFrom[A, B](implicit ev: A <-> B): To[B, A] = To(ev.from)
}

trait ToLowPriorityImplicits0 {
  implicit def toCompose[A, B, C](implicit
    ev0: A -> B,
    ev1: B -> C): To[A, C] = To[A, C](ev1.to compose ev0.to)
}
