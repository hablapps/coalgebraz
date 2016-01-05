package org.hablapps.coalgebraz

import scalaz._, Scalaz._

import org.hablapps.coalgebraz

object Coalgebra {

  def always[A](value: A): Coentity[Unit, Void, A, A] =
    _ => Entity(value, _ => (List.empty, Option(value)))

  def constant[A]: Coentity[Unit, Void, A, A] =
    s => Entity(s, _ => (List.empty, Option(s)))

  def apart[I1, I2, I, O1, O2, O, B1, B2, B, X1, X2](
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
  def together[I1, I2, O1, O2, B1, B2, X1, X2](
    co1: Coentity[I1, O1, B1, X1],
    co2: Coentity[I2, O2, B2, X2]): Coentity[(I1, I2), (O1, O2), (B1, B2), (X1, X2)] = ???
}
