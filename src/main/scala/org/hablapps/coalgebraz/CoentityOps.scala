package org.hablapps.coalgebraz

import scala.language.implicitConversions

import scalaz._, Scalaz._, Isomorphism.<=>

import Coalgebra._

class CoentityOps[I1, O1, B1, X1](val co1: Coentity[I1, O1, B1, X1]) {

  def |+|[I2, I, O2, O, B2, B, X2](co2: Coentity[I2, O2, B2, X2])(implicit
    ev0: ClearSum.Aux[I1, I2, I],
    ev1: ClearSum.Aux[O1, O2, O],
    ev2: ClearProduct.Aux[B1, B2, B]) = putApart(co1, co2)

  def |*|[I2, O2, B2, X2](co2: Coentity[I2, O2, B2, X2]) = putTogether(co1, co2)

  def withState[X2](implicit iso: X1 <=> X2): Coentity[I1, O1, B1, X2] =
    Coalgebra.withState[I1, O1, B1, X1, X2](co1)

  def withObservable[B2](implicit iso: B1 <=> B2) =
    Coalgebra.withObservable[I1, O1, B1, X1, B2](co1)

  def routeIn[I2](f: B1 => I2 => List[I1]): Coentity[I2, O1, B1, X1] =
    Coalgebra.routeIn[I1, O1, B1, X1, I2](f, co1)

  def routeOut[O2](f: B1 => O1 => List[O2]): Coentity[I1, O2, B1, X1] =
    Coalgebra.routeOut[I1, O1, B1, X1, O2](f, co1)

  def /+\[I2, I, O2, O](
      co2: Coentity[I2, O2, B1, X1])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O]) =
    Coalgebra.union[I1, I2, I, O1, O2, O, B1, X1](co1, co2)
}

object CoentityOps {
  implicit def toCoentityOps[I, O, B, X](
    co: Coentity[I, O, B, X]): CoentityOps[I, O, B, X] = new CoentityOps(co)
}
