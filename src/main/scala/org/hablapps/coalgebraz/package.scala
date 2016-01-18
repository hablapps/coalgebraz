package org.hablapps

import scala.language.higherKinds

import scalaz._, Scalaz._, Isomorphism.<=>

package object coalgebraz {

  /* Shared aliases */

  type Coalgebra[F[_], S] = S => F[S]

  type Costream[H, X] = Coalgebra[({type λ[α] = Stream[H, α]})#λ, X]

  type Coistream[H, X] = Coalgebra[({type λ[α] = IStream[H, α]})#λ, X]

  type Coautomata[I, O, X] = Coalgebra[({type λ[α] = Automata[I, O, α]})#λ, X]

  type Coiautomata[I, O, X] = Coalgebra[({type λ[α] = IAutomata[I, O, α]})#λ, X]

  type Costore[K, V, X] = Coalgebra[({type λ[α] = Store[K, V, α]})#λ, X]

  type Coistore[K, V, X] = Coalgebra[({type λ[α] = IStore[K, V, α]})#λ, X]

  type Coentity[I, O, C, X] = Coalgebra[({type λ[α] = Entity[I, O, C, α]})#λ, X]

  type Coientity[I, O, C, X] = Coalgebra[({type λ[α] = IEntity[I, O, C, α]})#λ, X]

  // XXX: using `type Void = Nothing` resulted in multiple errors because of the
  // problems with implicit resolutions when `Nothing` is involved.
  trait Void { ??? } // non-instantiable!

  type CoentitySeq[I, O, B, X] =
    Coentity[CoseqIn[I, B, X], CoseqOut[O, X], List[B], List[X]]

  type  ->[A, B] = To[A, B]
  type <->[A, B] = Iso[A, B]

  type Router[B, A1, A2] = B => A1 => List[A2]

  /* Generic combinators */

  // XXX: seems like a comonad!?!?
  implicit class Tuple2Helper[A, B](t2: Tuple2[A, B]) {
    def extend[C](f: Tuple2[A, B] => C): Tuple2[A, C] = t2 match {
      case (a, _) => (a, f(t2))
    }
  }

  def zip3[A, B, C](
      la: List[A],
      lb: List[B],
      lc: List[C]): List[(A, B, C)] = (la, lb, lc) match {
    case (Nil, _, _) | (_, Nil, _) | (_, _, Nil) => List.empty
    case (a::as, b::bs, c::cs) => (a, b, c) :: zip3(as, bs, cs)
  }
}
