package org.hablapps.coalgebraz

import scalaz._, Scalaz._

case class Stream[H, X](head: H, tail: Option[X]) {
  def map[Y](f: X => Y): Stream[H, Y] = copy(tail = tail.map(f))
}

case class IStream[H, X](head: H, tail: X) {
  def map[Y](f: X => Y): IStream[H, Y] = copy(tail = f(tail))
}

case class Automata[I, O, X](transition: I => Option[(O, X)]) {
  def map[Y](f: X => Y): Automata[I, O, Y] =
    Automata(i => transition(i).map(_ map f))
}

case class IAutomata[I, O, X](transition: I => (O, X)) {
  def map[Y](f: X => Y): IAutomata[I, O, Y] =
    IAutomata(i => transition(i) map f)
}

case class Store[K, V, X](get: V, set: K => Option[X]) {
  def map[Y](f: X => Y): Store[K, V, Y] = copy(set = k => set(k) map f)
}

case class IStore[K, V, X](get: V, set: K => X) {
  def map[Y](f: X => Y): IStore[K, V, Y] = copy(set = k => f(set(k)))
}

case class Entity[I, O, B, X](observe: B, next: I => (List[O], Option[X])) {
  def map[Y](f: X => Y): Entity[I, O, B, Y] =
    copy(next = i => next(i).map(_ map f))
}

case class IEntity[I, O, B, X](observe: B, next: I => (List[O], X)) {
  def map[Y](f: X => Y): IEntity[I, O, B, Y] =
    copy(next = i => next(i) map f)
}

object Codata {

  implicit def StreamFunctor[H] = new Functor[({type λ[α] = Stream[H, α]})#λ] {
    def map[A, B](r: Stream[H, A])(f: A => B) = r map f
  }

  implicit def IStreamFunctor[H] = new Functor[({type λ[α] = IStream[H, α]})#λ] {
    def map[A, B](r: IStream[H, A])(f: A => B) = r map f
  }

  implicit def AutomataFunctor[I, O] = new Functor[({type λ[α] = Automata[I, O, α]})#λ] {
    def map[A, B](r: Automata[I, O, A])(f: A => B) = r map f
  }

  implicit def IAutomataFunctor[I, O] = new Functor[({type λ[α] = IAutomata[I, O, α]})#λ] {
    def map[A, B](r: IAutomata[I, O, A])(f: A => B) = r map f
  }

  implicit def StoreFunctor[K, V] = new Functor[({type λ[α] = Store[K, V, α]})#λ] {
    def map[A, B](r: Store[K, V, A])(f: A => B) = r map f
  }

  implicit def IStoreFunctor[K, V] = new Functor[({type λ[α] = IStore[K, V, α]})#λ] {
    def map[A, B](r: IStore[K, V, A])(f: A => B) = r map f
  }

  implicit def EntityFunctor[I, O, C] = new Functor[({type λ[α] = Entity[I, O, C, α]})#λ] {
    def map[A, B](r: Entity[I, O, C, A])(f: A => B) = r map f
  }

  implicit def IEntityFunctor[I, O, C] = new Functor[({type λ[α] = IEntity[I, O, C, α]})#λ] {
    def map[A, B](r: IEntity[I, O, C, A])(f: A => B) = r map f
  }
}
