package coalgebraz

import scalaz._, Scalaz._, Scalaz.{ Identity => _ }

import Coalgebraz._

class StreamF[H, X](val head: H, _tail: => X) {
  def tail: X = _tail
  def map[Y](f: X => Y): StreamF[H, Y] = StreamF(head, f(tail))
}

object StreamF {

  def apply[H, X](head: H, tail: => X): StreamF[H, X] =
    new StreamF[H, X](head, tail)

  def unapply[H, X](sf: StreamF[H, X]): Option[(H, X)] =
    Option(sf.head, sf.tail)
}

case class MooreF[I, O, X](output: O, next: I => X) {
  def map[Y](f: X => Y): MooreF[I, O, Y] =
    MooreF(output, f compose next)
}

case class MealyF[I, O, X](next: I => (O, X)) {
  def map[Y](f: X => Y): MealyF[I, O, Y] =
    MealyF(next andThen (_ map f))
}

case class ObjectF[I, O, E, X](method: I => E \/ (O, X)) {
  def map[Y](f: X => Y): ObjectF[I, O, E, Y] =
    ObjectF(i => method(i).map(_ map f))
}

case class TransitionSystemF[X](next: List[X]) {
  def map[Y](f: X => Y): TransitionSystemF[Y] =
    TransitionSystemF(next map f)
}

case class MilnerF[A, X](next: List[(A, X)]) {
  def map[Y](f: X => Y): MilnerF[A, Y] =
    MilnerF(next map (_ map f))
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

  implicit def fromObjectF[I, O, E, X](
      co: Object[I, O, E, X]): Entity[I, E \/ O, Unit, X] = { s =>
    val ObjectF(method) = co(s)
    EntityF((), i => method(i).fold(
      e => halt ~> e.left[O],
      ox => ox._2 ~> ox._1.right[E]
    ))
  }
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

  implicit def ObjectFFunctor[I, O, E] = new Functor[ObjectF[I, O, E, ?]] {
    def map[A, B](r: ObjectF[I, O, E, A])(f: A => B) = r map f
  }

  implicit def TransitionSystemF = new Functor[TransitionSystemF[?]] {
    def map[A, B](r: TransitionSystemF[A])(f: A => B) = r map f
  }

  implicit def MilnerF[A] = new Functor[MilnerF[A, ?]] {
    def map[B, C](r: MilnerF[A, B])(f: B => C) = r map f
  }

  implicit def EntityFFunctor[I, O, C] = new Functor[EntityF[I, O, C, ?]] {
    def map[A, B](r: EntityF[I, O, C, A])(f: A => B) = r map f
  }
}
