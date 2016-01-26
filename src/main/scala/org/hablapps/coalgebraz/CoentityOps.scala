package org.hablapps.coalgebraz

import scala.language.implicitConversions

import scalaz._, Scalaz._

class EntityOps[I1, O1, B1, X1](val co1: Entity[I1, O1, B1, X1]) {

  def |~|(
      f: I1 => Boolean,
      g: B1 => I1 => List[O1] = (_: B1) => (_: I1) => List.empty[O1]) =
    Coalgebraz.untilOut(f, g)(co1)

  def andthen(co2: Entity[I1, O1, B1, X1]) =
    Coalgebraz.andthen(co1, co2)

  def until(f: I1 => Boolean) = Coalgebraz.until(f)(co1)

  def untilOut(
      f: I1 => Boolean,
      g: B1 => I1 => List[O1] = (_: B1) => (_: I1) => List.empty[O1]) =
    Coalgebraz.untilOut(f, g)(co1)

  def untilAndNext(f: I1 => Boolean)(co2: Entity[I1, O1, B1, X1]) =
    Coalgebraz.untilAndNext(f)(co1, co2)

  def interleave(co2: Entity[I1, O1, B1, X1]) =
    Coalgebraz.interleave(co1, co2)

  def block = Coalgebraz.block(co1)

  def stop = Coalgebraz.stop(co1)

  def stopOut(f: B1 => I1 => List[O1]) =
    Coalgebraz.stopOut(f)(co1)

  def withState[X2](implicit ev0: X1 <-> X2) =
    Coalgebraz.withState(co1)

  def withObservable[B2](implicit ev0: B1 -> B2) =
    Coalgebraz.withObservable(co1)

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

  def |*|[I2, I, O2, O, B2, B, X2, X](
      co2: Entity[I2, O2, B2, X2])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[B1, B2, B],
      ev3: ClearProduct.Aux[X1, X2, X]) =
    Coalgebraz.coexist(co1, co2)

  def \*/[I2, I, O2, O, B2, B](
      co2: Entity[I2, O2, B2, X1])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[B1, B2, B]) =
    Coalgebraz.fusion(co1, co2)

  def |->|[O2, B, B2, X, X2](
      co2: Entity[O1, O2, B2, X2])(implicit
      ev0: ClearProduct.Aux[B1, B2, B],
      ev1: ClearProduct.Aux[X1, X2, X]) =
    Coalgebraz.flow(co1, co2)
}

object EntityOps {
  implicit def toEntityOps[I, O, B, X](
    co: Entity[I, O, B, X]): EntityOps[I, O, B, X] = new EntityOps(co)
}
