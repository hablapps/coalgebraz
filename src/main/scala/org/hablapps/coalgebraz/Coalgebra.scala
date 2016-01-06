package org.hablapps.coalgebraz

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
    iso: X <=> X2): Coentity[I, O, B, X2] = ???

  def withObservable[I, O, B, X, B2](
    co: Coentity[I, O, B, X])(implicit
    iso: B <=> B2): Coentity[I, O, B2, X] = ???

  // TODO: I need something like 'feed' to be able to implement this.
  def routeIn[I, O, B, X, I2](
      f: B => I2 => List[I],
      co: Coentity[I, O, B, X]): Coentity[I2, O, B, X] = ???

  def routeOut[I, O, B, X, O2](
      f: B => O => List[O2],
      co: Coentity[I, O, B, X]): Coentity[I, O2, B, X] = { x =>
    val Entity(obs, nxt) = co(x)
    Entity(obs, i => nxt(i).swap.map(_ flatMap f(obs)).swap)
  }

  def union[I1, I2, I, O1, O2, O, B, X](
    co1: Coentity[I1, O1, B, X],
    co2: Coentity[I2, O2, B, X])(implicit
    ev0: ClearSum.Aux[I1, I2, I],
    ev1: ClearSum.Aux[O1, O2, O]): Coentity[I, O, B, X] = ???
}
