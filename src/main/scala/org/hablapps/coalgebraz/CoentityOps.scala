package org.hablapps.coalgebraz

import scala.language.implicitConversions

import scalaz._, Scalaz._, Isomorphism.<=>

import Coalgebraz._

class CoentityOps[I1, O1, B1, X1](val co1: Coentity[I1, O1, B1, X1]) {

  def |+|[I2, I, O2, O, B2, B, X2](co2: Coentity[I2, O2, B2, X2])(implicit
    ev0: ClearSum.Aux[I1, I2, I],
    ev1: ClearSum.Aux[O1, O2, O],
    ev2: ClearProduct.Aux[B1, B2, B]) = putApart(co1, co2)

  def |*|[I2, O2, B2, X2](co2: Coentity[I2, O2, B2, X2]) = putTogether(co1, co2)

  def withState[X2](implicit ev0: X1 <-> X2) =
    Coalgebraz.withState[I1, O1, B1, X1, X2](co1)

  def withObservable[B2](implicit ev0: B1 -> B2) =
    Coalgebraz.withObservable[I1, O1, B1, X1, B2](co1)

  def routeIn[I2](implicit r: Router[B1, I2, I1]) =
    Coalgebraz.routeIn[I1, O1, B1, X1, I2](co1)

  def routeOut[O2](implicit r: Router[B1, O1, O2]) =
    Coalgebraz.routeOut[I1, O1, B1, X1, O2](co1)

  def routeBack(implicit r: Router[B1, O1, I1]) =
    Coalgebraz.routeBack[I1, O1, B1, X1](co1)

  def /+\[I2, I, O2, O](
      co2: Coentity[I2, O2, B1, X1])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O]) =
    Coalgebraz.union[I1, I2, I, O1, O2, O, B1, X1](co1, co2)

  def toCoseq(implicit sq: Sq[List, Option]) =
    Coalgebraz.toCoseq[I1, O1, B1, X1](co1)(sq)

  def |->|[O2, B, B2, X, X2](
      co2: Coentity[O1, O2, B2, X2])(implicit
      ev0: ClearProduct.Aux[B1, B2, B],
      ev1: ClearProduct.Aux[X1, X2, X],
      ev2: ((X1, X2) <-> X)) =
    Coalgebraz.flow[O1, I1, O2, B, B1, B2, X, X1, X2](co1, co2)
}

object CoentityOps {
  implicit def toCoentityOps[I, O, B, X](
    co: Coentity[I, O, B, X]): CoentityOps[I, O, B, X] = new CoentityOps(co)
}
