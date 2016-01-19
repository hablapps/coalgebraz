package org.hablapps.coalgebraz

import scala.language.implicitConversions

import scalaz._, Scalaz._

class CoentityOps[I1, O1, B1, X1](val co1: Coentity[I1, O1, B1, X1]) {

  def withState[X2](implicit ev0: X1 <-> X2) =
    Coalgebraz.withState(co1)

  def withObservable[B2](implicit ev0: B1 -> B2) =
    Coalgebraz.withObservable(co1)

  def withInput[I2](implicit ev0: I2 -> I1) =
    Coalgebraz.withInput(co1)

  def withOutput[O2](implicit ev0: O1 -> O2) =
    Coalgebraz.withOutput(co1)

  def routeIn[I2](implicit r: Router[B1, I2, I1]) =
    Coalgebraz.routeIn(co1)

  def routeOut[O2](implicit r: Router[B1, O1, O2]) =
    Coalgebraz.routeOut(co1)

  def routeBack(implicit r: Router[B1, O1, I1]) =
    Coalgebraz.routeBack(co1)

  def outputFromBehaviour(f: B1 => List[O1]) =
    Coalgebraz.outputFromBehaviour(co1)(f)

  def toCoseq(implicit sq: Sq[List, Option]) =
    Coalgebraz.toCoseq(co1)

  def |+|[I2, I, O2, O, B2, B, X2](
      co2: Coentity[I2, O2, B2, X2])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[B1, B2, B]) =
    Coalgebraz.coexist(co1, co2)

  def \+/[I2, I, O2, O, B2, B](
      co2: Coentity[I2, O2, B2, X1])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[B1, B2, B]) =
    Coalgebraz.fusion(co1, co2)

  def |->|[O2, B, B2, X, X2](
      co2: Coentity[O1, O2, B2, X2])(implicit
      ev0: ClearProduct.Aux[B1, B2, B],
      ev1: ClearProduct.Aux[X1, X2, X],
      ev2: ((X1, X2) <-> X)) =
    Coalgebraz.flow(co1, co2)
}

object CoentityOps {
  implicit def toCoentityOps[I, O, B, X](
    co: Coentity[I, O, B, X]): CoentityOps[I, O, B, X] = new CoentityOps(co)
}
