package coalgebraz

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import Coalgebraz._

class StreamF[H, X](val head: H, _tail: => X) {
  def tail: X = _tail
  def map[Y](f: X => Y): StreamF[H, Y] = StreamF(head, f(tail))
  def bimap[J, Y](f: H => J, g: X => Y) = StreamF(f(head), tail) map g
}

object StreamF {

  def apply[H, X](head: H, tail: => X): StreamF[H, X] =
    new StreamF[H, X](head, tail)

  def unapply[H, X](sf: StreamF[H, X]): Option[(H, X)] =
    Option(sf.head, sf.tail)
}

object CoalgebraTypeclass extends App {

  import shapeless._

  trait CoalgAlg[F[_]] {

    // XXX: we don't need anything else (but the coalgebra itself) to make
    // `merge` work!!!
    def merge[X, Y](
        co1: Coalgebra[F, X],
        co2: Coalgebra[F, Y])(implicit
        ev0: Functor[F]): Coalgebra[F, (X, Y) :+: (Y, X) :+: CNil] = {
      type R = (X, Y) :+: (Y, X) :+: CNil
      object toStream extends Poly1 {
        implicit val caseXY = at[(X, Y)] { case (x, y) =>
          co1(x).map(Coproduct[R](y, _))
        }
        implicit val caseYX = at[(Y, X)] { case (y, x) =>
          co2(y).map(Coproduct[R](x, _))
        }
      }
      _.fold(toStream)
    }
  }

  // 1. Primera aproximación para elaborar una typeclass para Stream con sus
  // funciones observadoras. Sabemos cual es el tipo de una coalgebra, por lo
  // que lo único que debería estar parametrizado es el functor y el valor
  // observable (que asumiremos constante). Una signatura del tipo `Coalgebra[F,
  // X] => A` no es viable, ya que requerimos una `X` para avanzar el estado.
  // Por tanto, para hacer que las cosas compilen, llegamos a esta solución.

  trait Stream[F[_], A] {

    def head[X](coalg: Coalgebra[F, X]): X => A

    def tail[X](coalg: Coalgebra[F, X]): X => X
  }

  trait StreamAlg[F[_], A] extends CoalgAlg[F] {

    def stream[X](pi1: X => A, pi2: X => X): Coalgebra[F, X]

    def zipWith[X, Y](
        f: (A, A) => A)(
        s1: Coalgebra[F, X],
        s2: Coalgebra[F, Y])(implicit
        ev0: Stream[F, A]): Coalgebra[F, (X, Y)] =
      stream(
        xy => f(ev0.head(s1)(xy._1), ev0.head(s2)(xy._2)),
        xy => (ev0.tail(s1)(xy._1), ev0.tail(s2)(xy._2)))
  }

  trait StreamInstance[A] extends Stream[StreamF[A, ?], A] {

    def head[X](coalg: Coalgebra[StreamF[A, ?], X]): X => A = coalg(_).head

    def tail[X](coalg: Coalgebra[StreamF[A, ?], X]): X => X = coalg(_).tail
  }

  trait StreamAlgInstance[A] extends StreamAlg[StreamF[A, ?], A] {
    def stream[X](pi1: X => A, pi2: X => X): Coalgebra[StreamF[A, ?], X] =
      x => StreamF(pi1(x), pi2(x))
  }

  def expr1[F[_]](implicit
      ev0: StreamAlg[F, Int],
      ev1: Stream[F, Int]): Coalgebra[F, (Int, Int)] = {
    import ev0._
    val nats = stream[Int](identity, _ + 1)
    zipWith(_ + _)(nats, nats)
  }

  trait MooreAutomata[F[_], I, O] {

    def out[X](coalg: Coalgebra[F, X]): X => O

    def next[X](coalg: Coalgebra[F, X]): X => I => X
  }

  trait MooreAutomataAlg[F[_], I, O] extends StreamAlg[F, O] {

    def moore[X](pi1: X => O, pi2: X => I => X): Coalgebra[F, X]

    def stream[X](pi1: X => O, pi2: X => X): Coalgebra[F, X] =
      moore(pi1, x => _ => pi2(x))
  }

  trait MooreInstance[I, O] extends MooreAutomata[MooreF[I, O, ?], I, O] {

    def out[X](coalg: Coalgebra[MooreF[I, O, ?], X]): X => O =
      coalg(_).output

    def next[X](coalg: Coalgebra[MooreF[I, O, ?], X]): X => I => X =
      coalg(_).next
  }

  trait MooreAlgInstance[I, O] extends MooreAutomataAlg[MooreF[I, O, ?], I, O] {
    def moore[X](pi1: X => O, pi2: X => I => X): Coalgebra[MooreF[I, O, ?], X] =
      x => MooreF(pi1(x), pi2(x))
  }

  def expr2[F[_]](implicit
        ev0: MooreAutomataAlg[F, Int, Boolean],
        ev1: MooreAutomata[F, Int, Boolean],
        ev2: Functor[F])
      : Coalgebra[F, (Boolean, Boolean) :+: (Boolean, Boolean) :+: CNil] = {
    import ev0._
    val odds = moore[Boolean](x => x.fold(false, true), x => _ => ! x)
    merge(odds, odds)
  }

  implicit def mooreAlgInstance[I, O]: MooreAutomataAlg[MooreF[I, O, ?], I, O] =
    new MooreAlgInstance[I, O] {}

  implicit def mooreInstance[I, O]: MooreAutomata[MooreF[I, O, ?], I, O] =
    new MooreInstance[I, O] {}

  runIO(expr2[MooreF[Int, Boolean, ?]](
    mooreAlgInstance,
    mooreInstance,
    MooreFFunctor))(Coproduct(false, false), o => println(s"⇒ $o"))

  // 2. ¿Es posible quitar el parámetro `A` de la typeclass Stream? Parece que
  // sí, pero nos encontramos con problemas a la hora de instanciarla con
  // `StreamF`. En tal caso se requeriría recibir un parámetro tipo en la
  // instancia. Se eleva un conflicto de tipos. Incluir un tipo existencial
  // tampoco parece arreglar el problema.
  //
  // trait Stream[F[_]] {
  //
  //   def head[A, X](coalg: Coalgebra[F, X]): X => A
  //
  //   def tail[A, X](coalg: Coalgebra[F, X]): X => X
  // }
  //
  // import scala.language.existentials
  //
  // trait StreamInstance extends Stream[StreamF[_, ?]] {
  //
  //   def head[A, X](coalg: Coalgebra[StreamF[A, ?], X]): X => A = coalg(_).head
  //
  //   def tail[A, X](coalg: Coalgebra[StreamF[A, ?], X]): X => X = coalg(_).tail
  // }


  // 3. Estamos haciendo que la typeclass sea demasiado partícipe de la
  // existencia de una estructura y un carrier, propias de las F-coalgebras.
  // ¿Podemos abstraer tal información? ¿Tiene sentido? => Podemos llegar a una
  // solución de este estilo, en la que no existen coalgebras, simplemente un
  // constructor parametrizado por un estado. No obstante, no creo que aporte
  // demasiado, ya que nuestra intención es clasificar máquinas, y una máquina
  // es una coalgebra, que ya es una abstracción suficientemente genérica.
  //
  // trait Stream[C[_], A] {
  //
  //   def head[X](coalg: C[X]): X => A
  //
  //   def tail[X](coalg: C[X]): X => X
  // }
  //
  // trait StreamInstance[A] extends Stream[Coalgebra[StreamF[A, ?], ?], A] {
  //
  //   def head[X](coalg: Coalgebra[StreamF[A, ?], X]): X => A = coalg(_).head
  //
  //   def tail[X](coalg: Coalgebra[StreamF[A, ?], X]): X => X = coalg(_).tail
  // }


  // 4. Parece que se están encontrando problemas a la hora de conseguir los
  // valores de las observadoras simplemente contando con un sistema. Tiene todo
  // el sentido del mundo, ya que la única forma de hacer avanzar una máquina es
  // mediante un estado. ¿Tiene sentido introducir el concepto de proceso y
  // trabajar sobre él?
  //
  // case class Process[F[_], X](system: Coalgebra[F, X], state: X) {
  //   def apply: F[X] = system(state)
  // }
  //
  // trait Stream[F[_], A] {
  //
  //   def head[X](proc: Process[F, X]): A
  //
  //   def tail[X](proc: Process[F, X]): X
  // }
  //
  // trait StreamInstance[A] extends Stream[StreamF[A, ?], A] {
  //
  //   def head[X](proc: Process[StreamF[A, ?], X]): A = proc.apply.head
  //
  //   def tail[X](proc: Process[StreamF[A, ?], X]): X = proc.apply.tail
  // }
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

class CCSF[A, X](_next: => LazyList[(A \/ A, X)]) {

  def next: LazyList[(A \/ A, X)] = _next

  def map[Y](f: X => Y): CCSF[A, Y] =
    CCSF(next map (_ map f))
}

object CCSF {

  def apply[A, X](next: => LazyList[(A \/ A, X)]): CCSF[A, X] =
    new CCSF(next)

  def unapply[A, X](mf: CCSF[A, X]): Option[LazyList[(A \/ A, X)]] =
    Option(mf.next)
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

  implicit def StreamFBifunctor = new Bifunctor[StreamF] {
    def bimap[A, B, C, D](
        r: StreamF[A, B])(
        f: A => C, g: B => D): StreamF[C, D] =
      r bimap (f, g)
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

  implicit def CCSF[A] = new Functor[CCSF[A, ?]] {
    def map[B, C](r: CCSF[A, B])(f: B => C) = r map f
  }

  implicit def EntityFFunctor[I, O, C] = new Functor[EntityF[I, O, C, ?]] {
    def map[A, B](r: EntityF[I, O, C, A])(f: A => B) = r map f
  }
}
