import scalaz._, Scalaz._

package object coalgebraz {

  import Coalgebraz._

  /* Shared aliases */

  type Coalgebra[F[_], S] = S => F[S]

  type Entity[I, O, B, X] = Coalgebra[EntityF[I, O, B, ?], X]

  type IEntity[I, O, B, X] = Coalgebra[IEntityF[I, O, B, ?], X]

  type Stream[H, X] = Coalgebra[StreamF[H, ?], X]

  type Automata[I, O, X] = Coalgebra[AutomataF[I, O, ?], X]

  type IAutomata[I, O, X] = Coalgebra[IAutomataF[I, O, ?], X]

  type Store[K, V, X] = Coalgebra[StoreF[K, V, ?], X]

  type IStore[K, V, X] = Coalgebra[IStoreF[K, V, ?], X]

  type  ->[A, B] = To[A, B]
  type <->[A, B] = Iso[A, B]

  type Router[B, A1, A2] = B => A1 => List[A2]

  type Next[I, O, X] = I => (List[O], Option[X])

  /* Index Stuff (perhaps should be moved) */

  type IndexedEntity[I, O, B, X, N] =
    Entity[IndexIn[I, B, N], IndexOut[O, B, N], Map[N, B], List[X]]

  type IndexedEntity2[I, O, F[_, _], B, X, N] =
    Entity[IndexIn[I, B, N], IndexOut[O, B, N], F[N, B], F[N, X]]

  /* Implicit converters */

  implicit def idRouter[B, A]: Router[B, A, A] = _ => List(_)

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
