package org.hablapps.coalgebraz

import scala.language.implicitConversions

import Coalgebra._

class CoentityOps[I1, O1, B1, X1](val co1: Coentity[I1, O1, B1, X1]) {

  def |+|[I2, I, O2, O, B2, B, X2](co2: Coentity[I2, O2, B2, X2])(implicit
    ev0: ClearSum.Aux[I1, I2, I],
    ev1: ClearSum.Aux[O1, O2, O],
    ev2: ClearProduct.Aux[B1, B2, B]) = putApart(co1, co2)

  def |*|[I2, O2, B2, X2](co2: Coentity[I2, O2, B2, X2]) = putTogether(co1, co2)
}

object CoentityOps {
  implicit def toCoentityOps[I, O, B, X](
    co: Coentity[I, O, B, X]): CoentityOps[I, O, B, X] = new CoentityOps(co)
}