package coalgebraz

import scalaz._, Scalaz._

trait Iso[A, B] extends To[A, B] {
  val from: B => A
}

object Iso extends IsoImplicits {
  def apply[A, B](to2: A => B, from2: B => A): Iso[A, B] = new Iso[A, B] {
    val to = to2
    val from = from2
  }
}

trait IsoImplicits extends IsoLowPriorityImplicits3 {
  implicit def isoTupleNest[A, B, C] = Iso[((A, B), C), (A, B, C)](
    { case ((a, b), c) => (a, b, c) },
    { case (a, b, c) => ((a, b), c)})
}

trait IsoLowPriorityImplicits3 extends IsoLowPriorityImplicits2 {

  implicit def isoAxUnit[A] = Iso[(A, Unit), A](
    { case (a, _) => a },
    (_, ()))

  implicit def isoAorVoid[A] = Iso[A \/ Void, A](
    { case -\/(a) => a; case \/-(_) => ??? },
    -\/(_))

  implicit def isoTupleAssoc[A, B, C] = Iso[(A, (B, C)), ((A, B), C)](
    { case (a, (b, c)) => ((a, b), c) },
    { case ((a, b), c) => (a, (b, c)) })
}

trait IsoLowPriorityImplicits2 extends IsoLowPriorityImplicits1 {

  implicit def isoTuple2[A, B] = Iso[(A, B), (B, A)](_.swap, _.swap)

  implicit def isoEqual[A] = Iso[A, A](identity, identity)
}

trait IsoLowPriorityImplicits1 extends IsoLowPriorityImplicits0 {
  implicit def isoFirst[A, B, C](implicit ev: A <-> C) = Iso[(A, B), (C, B)](
    { case (a, b) => (ev.to(a), b) },
    { case (c, b) => (ev.from(c), b) })
}

trait IsoLowPriorityImplicits0 {
  implicit def isoCompose[A, B, C](implicit
      ev0: A <-> B,
      ev1: B <-> C) = Iso[A, C](
    ev1.to compose ev0.to,
    ev0.from compose ev1.from)
}
