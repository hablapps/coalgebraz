package org.hablapps.coalgebraz

import scala.language.implicitConversions

import scalaz._, Scalaz._, Isomorphism.<=>

import org.hablapps.coalgebraz

object Coalgebra {

  def always[A](value: A): Coentity[Unit, Void, A, A] =
    _ => Entity(value, _ => (List.empty, Option(value)))

  def constant[A]: Coentity[Void, Void, A, A] =
    s => Entity(s, _ => ??? /* does never happen */)

  def putApart[I1, I2, I, O1, O2, O, B1, B2, B, X1, X2](
      co1: Coentity[I1, O1, B1, X1],
      co2: Coentity[I2, O2, B2, X2])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[B1, B2, B]): Coentity[I, O, B, (X1, X2)] = { x =>
    val (s, t) = x
    val Entity(obs1, nxt1) = co1(s)
    val Entity(obs2, nxt2) = co2(t)
    Entity(ev2(obs1, obs2), i => ev0(
      i1 => nxt1(i1).bimap(_.map(o => ev1(o.left)), _.map((_, t))),
      i2 => nxt2(i2).bimap(_.map(o => ev1(o.right)), _.map((s, _))), i))
  }

  // TODO: why not implementing this version, where both coalgebras evolve
  // simultaneously?
  def putTogether[I1, I2, O1, O2, B1, B2, X1, X2](
    co1: Coentity[I1, O1, B1, X1],
    co2: Coentity[I2, O2, B2, X2]): Coentity[(I1, I2), (O1, O2), (B1, B2), (X1, X2)] = ???

  def withState[I, O, B, X, X2](
      co: Coentity[I, O, B, X])(implicit
      iso: X <=> X2): Coentity[I, O, B, X2] = { x2 =>
    val Entity(obs, nxt) = co(iso.from(x2))
    Entity(obs, i => nxt(i).map(_ map iso.to))
  }

  def withObservable[I, O, B, X, B2](
      co: Coentity[I, O, B, X])(implicit
      iso: B <=> B2): Coentity[I, O, B2, X] = { x =>
    val Entity(obs, nxt) = co(x)
    Entity(iso.to(obs), nxt)
  }

  // Feeds a coalgebra with a list of inputs and returns the final state (if
  // any) along with the list of outputs that were generated by the system.
  def feed[I, O, B, X](
      co: Coentity[I, O, B, X],
      in: List[I],
      x: X): (List[O], Option[X]) = in match {
    case Nil => (List.empty, Option(x))
    case i::is => {
      val Entity(_, nxt) = co(x)
      nxt(i) match {
        case (os, Some(x2)) => feed(co, is, x2).mapElements(_1 = os ++ _)
        case (os, None) => (os, None)
      }
    }
  }

  // Adapts a system to map input broader events into smaller ones, given a
  // mapper function.
  def routeIn[I, O, B, X, I2](
      f: B => I2 => List[I],
      co: Coentity[I, O, B, X]): Coentity[I2, O, B, X] = { x =>
    val Entity(obs, nxt) = co(x)
    Entity(obs, i => feed(co, f(obs)(i), x))
  }

  // Adapts a system to map output smaller events into broader ones, given a
  // mapper function.
  def routeOut[I, O, B, X, O2](
      f: B => O => List[O2],
      co: Coentity[I, O, B, X]): Coentity[I, O2, B, X] = { x =>
    val Entity(obs, nxt) = co(x)
    Entity(obs, i => nxt(i).swap.map(_ flatMap f(obs)).swap)
  }

  // Permits two coalgebras to share the very same inner state. It worths
  // mentioning that the observation from the second coalgebra is discarded.
  def union[I1, I2, I, O1, O2, O, B, X](
      co1: Coentity[I1, O1, B, X],
      co2: Coentity[I2, O2, B, X])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O]): Coentity[I, O, B, X] = { x =>
    val Entity(obs1, nxt1) = co1(x)
    val Entity(_, nxt2) = co2(x)
    Entity(obs1, i => ev0(
      i1 => nxt1(i1).swap.map(os => os.map(o1 => ev1(o1.left))).swap,
      i2 => nxt2(i2).swap.map(os => os.map(o2 => ev1(o2.right))).swap,
      i))
  }

  sealed trait CoseqIn[I, B, X]
  case class ApplySuch[I, B, X](f: B => Option[I]) extends CoseqIn[I, B, X]
  case class Prepend[I, B, X](value: X) extends CoseqIn[I, B, X]

  sealed trait CoseqOut[O, X]
  case class WrappedOut[O, X](os: List[O]) extends CoseqOut[O, X]
  case class Prepended[O, X](value: X) extends CoseqOut[O, X]
  case class Removed[O, X](value: X) extends CoseqOut[O, X]

  type CoentitySeq[I, O, B, X] =
    Coentity[CoseqIn[I, B, X], CoseqOut[O, X], List[B], List[X]]

  // object Coseq {
  //
  //   implicit def allOrNone[A](loa: List[Option[A]]): Option[List[A]] =
  //     loa.sequence
  //
  //   implicit def someOrNone[A](loa: List[Option[A]]): Option[List[A]] = loa match {
  //     case Nil => None
  //     case (Some(x)::xs) => someOrNone(xs).map(x::_)
  //     case (None::xs) => someOrNone(xs)
  //   }
  // }

  def toCoseq[I, O, B, X](
      co: Coentity[I, O, B, X]): CoentitySeq[I, O, B, X] = { xs =>
    val es = xs map (co(_))
    val bs = es map (_.observe)
    val nx = es map (_.next)
    Entity(bs, _ match {
      case ApplySuch(f) => {
        val ois = bs map f
        ???
      }
      case Prepend(x) => (List(Prepended(x)), Option(x::xs))
    })
  }
}
