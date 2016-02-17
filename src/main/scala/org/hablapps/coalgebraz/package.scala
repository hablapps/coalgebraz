package org.hablapps

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._, Scalaz._

package object coalgebraz {

  import EntityOps._

  /* Shared aliases */

  type Coalgebra[F[_], S] = S => F[S]

  type Stream[H, X] = Coalgebra[StreamF[H, ?], X]

  type IStream[H, X] = Coalgebra[IStreamF[H, ?], X]

  type Automata[I, O, X] = Coalgebra[AutomataF[I, O, ?], X]

  type IAutomata[I, O, X] = Coalgebra[IAutomataF[I, O, ?], X]

  type Store[K, V, X] = Coalgebra[StoreF[K, V, ?], X]

  type IStore[K, V, X] = Coalgebra[IStoreF[K, V, ?], X]

  // XXX: is there a better name for `B`, more related to oBservable
  type Entity[I, O, B, X] = Coalgebra[EntityF[I, O, B, ?], X]

  type IEntity[I, O, B, X] = Coalgebra[IEntityF[I, O, B, ?], X]

  // XXX: using `type Void = Nothing` resulted in multiple errors because of the
  // problems with implicit resolutions when `Nothing` is involved.
  final class Void { ??? } // non-instantiable!

  type  ->[A, B] = To[A, B]
  type <->[A, B] = Iso[A, B]

  type Router[B, A1, A2] = B => A1 => List[A2]

  type Next[I, O, X] = I => (List[O], Option[X])

  /* Index Stuff (perhaps should be moved) */

  type IndexedEntity[I, O, B, X, N] =
    Entity[IndexIn[I, X, N], IndexOut[O, X, N], Map[N, B], List[X]]

  /* Implicit converters */

  implicit def idRouter[B, A]: Router[B, A, A] = _ => List(_)

  object Adapt extends AdaptLowPriorityImplicits {
    implicit def adapt2[I1, I2, O1, O2, B1, B2, X1, X2](
        co:  Entity[I1, O1, B1, X1])(implicit
        ev0: Router[B2, I2, I1],
        ev1: Router[B2, O1, O2],
        ev2: B1  -> B2,
        ev3: X1 <-> X2): Entity[I2, O2, B2, X2] =
      co.observe(ev2).in(ev0).out(ev1).carrier(ev3)
  }

  trait AdaptLowPriorityImplicits {
    implicit def adapt[I1, I2, O1, O2, B1, B2, X1, X2](
        co:  Entity[I1, O1, B1, X1])(implicit
        ev0: Router[B1, I2, I1],
        ev1: Router[B1, O1, O2],
        ev2: B1  -> B2,
        ev3: X1 <-> X2): Entity[I2, O2, B2, X2] =
      co.in(ev0).out(ev1).observe(ev2).carrier(ev3)
  }

  /* Generic combinators */

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
