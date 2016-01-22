package org.hablapps.coalgebraz

import scala.language.implicitConversions

import scalaz._, Scalaz._

case class StreamF[H, X](head: H, tail: Option[X]) {
  def map[Y](f: X => Y): StreamF[H, Y] = copy(tail = tail.map(f))
}

case class IStreamF[H, X](head: H, tail: X) {
  def map[Y](f: X => Y): IStreamF[H, Y] = copy(tail = f(tail))
}

case class AutomataF[I, O, X](transition: I => Option[(O, X)]) {
  def map[Y](f: X => Y): AutomataF[I, O, Y] =
    AutomataF(i => transition(i).map(_ map f))
}

case class IAutomataF[I, O, X](transition: I => (O, X)) {
  def map[Y](f: X => Y): IAutomataF[I, O, Y] =
    IAutomataF(i => transition(i) map f)
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
    EntityF(h, _ => (List.empty, t))
  }

  implicit def fromIStreamF[H, X](
      co: IStream[H, X]): Entity[Unit, Void, H, X] = { s =>
    val IStreamF(h, t) = co(s)
    EntityF(h, _ => (List.empty, Option(t)))
  }

  implicit def fromAutomataF[I, O, X](
      co: Automata[I, O, X]): Entity[I, O, Unit, X] = { s =>
    val AutomataF(tr) = co(s)
    EntityF((), i => tr(i).fold(
      (List.empty[O], None: Option[X])) { case (o, x) => (List(o), Option(x)) })
  }

  implicit def fromIAutomataF[I, O, X](
      co: IAutomata[I, O, X]): Entity[I, O, Unit, X] = { s =>
    val IAutomataF(tr) = co(s)
    EntityF((), i => tr(i).mapElements(List(_), Option.apply))
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

object Codata {

  implicit def StreamFFunctor[H] = new Functor[({type λ[α] = StreamF[H, α]})#λ] {
    def map[A, B](r: StreamF[H, A])(f: A => B) = r map f
  }

  implicit def IStreamFFunctor[H] = new Functor[({type λ[α] = IStreamF[H, α]})#λ] {
    def map[A, B](r: IStreamF[H, A])(f: A => B) = r map f
  }

  implicit def AutomataFFunctor[I, O] = new Functor[({type λ[α] = AutomataF[I, O, α]})#λ] {
    def map[A, B](r: AutomataF[I, O, A])(f: A => B) = r map f
  }

  implicit def IAutomataFFunctor[I, O] = new Functor[({type λ[α] = IAutomataF[I, O, α]})#λ] {
    def map[A, B](r: IAutomataF[I, O, A])(f: A => B) = r map f
  }

  implicit def StoreFFunctor[K, V] = new Functor[({type λ[α] = StoreF[K, V, α]})#λ] {
    def map[A, B](r: StoreF[K, V, A])(f: A => B) = r map f
  }

  implicit def IStoreFFunctor[K, V] = new Functor[({type λ[α] = IStoreF[K, V, α]})#λ] {
    def map[A, B](r: IStoreF[K, V, A])(f: A => B) = r map f
  }

  implicit def EntityFFunctor[I, O, C] = new Functor[({type λ[α] = EntityF[I, O, C, α]})#λ] {
    def map[A, B](r: EntityF[I, O, C, A])(f: A => B) = r map f
  }

  implicit def IEntityFFunctor[I, O, C] = new Functor[({type λ[α] = IEntityF[I, O, C, α]})#λ] {
    def map[A, B](r: IEntityF[I, O, C, A])(f: A => B) = r map f
  }
}
