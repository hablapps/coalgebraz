package coalgebraz

import scalaz._, Scalaz._

import Coalgebraz._

case class StreamF[H, X](head: H, tail: X) {
  def map[Y](f: X => Y): StreamF[H, Y] = StreamF(head, f(tail))
}

case class MooreF[I, O, X](output: O, next: I => X) {
  def map[Y](f: X => Y): MooreF[I, O, Y] =
    MooreF(output, f compose next)
}

case class MealyF[I, O, X](next: I => (O, X)) {
  def map[Y](f: X => Y): MealyF[I, O, Y] =
    MealyF(next andThen (_ map f))
}

case class StoreF[K, V, X](get: V, set: K => Option[X]) {
  def map[Y](f: X => Y): StoreF[K, V, Y] = copy(set = k => set(k) map f)
}

case class IStoreF[K, V, X](get: V, set: K => X) {
  def map[Y](f: X => Y): IStoreF[K, V, Y] = copy(set = k => f(set(k)))
}

case class EntityF[I, O, B, X](observe: B, next: I => (List[O], Option[X])) {
  def map[Y](f: X => Y): EntityF[I, O, B, Y] =
    copy(next = i => next(i).map(_ map f))
}

object EntityF {

  implicit def fromStreamF[H, X](
      co: Stream[H, X]): Entity[Unit, Void, H, X] = { s =>
    val StreamF(h, t) = co(s)
    EntityF(h, _ => (List.empty, Option(t)))
  }

  implicit def fromMooreF[I, O, X](
      co: Moore[I, O, X]): Entity[I, Void, O, X] = { s =>
    val MooreF(o, nxt) = co(s)
    EntityF(o, i => (List.empty, Option(nxt(i))))
  }

  implicit def fromMealyF[I, O, X](
      co: Mealy[I, O, X]): Entity[I, O, Unit, X] = { s =>
    val MealyF(nxt) = co(s)
    EntityF((), i => nxt(i).bimap(List.apply[O], Option.apply))
  }

  implicit def fromStoreF[K, V, X](
      co: Store[K, V, X]): Entity[K, Void, V, X] = { s =>
    val StoreF(get, set) = co(s)
    EntityF(get, i => (List.empty, set(i)))
  }

  implicit def fromIStoreF[K, V, X](
      co: IStore[K, V, X]): Entity[K, Void, V, X] = { s =>
    val IStoreF(get, set) = co(s)
    EntityF(get, i => (List.empty, Option(set(i))))
  }

  implicit def fromIEntityF[I, O, B, X](
      co: IEntity[I, O, B, X]): Entity[I, O, B, X] = { s =>
    val IEntityF(obs, nxt) = co(s)
    EntityF(obs, i => nxt(i).map(Option.apply))
  }
}

case class IEntityF[I, O, B, X](observe: B, next: I => (List[O], X)) {
  def map[Y](f: X => Y): IEntityF[I, O, B, Y] =
    copy(next = i => next(i) map f)
}

trait CodataInstances {

  implicit def StreamFFunctor[H] = new Functor[StreamF[H, ?]] {
    def map[A, B](r: StreamF[H, A])(f: A => B) = r map f
  }

  implicit def MooreFFunctor[I, O] = new Functor[MooreF[I, O, ?]] {
    def map[A, B](r: MooreF[I, O, A])(f: A => B) = r map f
  }

  implicit def MealyFFunctor[I, O] = new Functor[MealyF[I, O, ?]] {
    def map[A, B](r: MealyF[I, O, A])(f: A => B) = r map f
  }

  implicit def StoreFFunctor[K, V] = new Functor[StoreF[K, V, ?]] {
    def map[A, B](r: StoreF[K, V, A])(f: A => B) = r map f
  }

  implicit def IStoreFFunctor[K, V] = new Functor[IStoreF[K, V, ?]] {
    def map[A, B](r: IStoreF[K, V, A])(f: A => B) = r map f
  }

  implicit def EntityFFunctor[I, O, C] = new Functor[EntityF[I, O, C, ?]] {
    def map[A, B](r: EntityF[I, O, C, A])(f: A => B) = r map f
  }

  implicit def IEntityFFunctor[I, O, C] = new Functor[IEntityF[I, O, C, ?]] {
    def map[A, B](r: IEntityF[I, O, C, A])(f: A => B) = r map f
  }
}
