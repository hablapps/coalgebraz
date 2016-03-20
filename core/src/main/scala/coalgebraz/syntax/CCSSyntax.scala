package coalgebraz
package syntax

import scalaz._, Scalaz._

import shapeless._, shapeless.{ :+:, Coproduct }
import ops.coproduct._, ops.coproduct.{ Inject }

import Coalgebraz._

class CCSOps[A1, X1 <: Coproduct](val self: CCS[A1, X1]) {

  def +[X2 <: Coproduct, HX1, HX2, TX1 <: Coproduct, TX2 <: Coproduct](
      m2: CCS[A1, X2])(implicit
      ev0: IsCCons.Aux[X1, HX1, TX1],
      ev1: IsCCons.Aux[X2, HX2, TX2],
      ev2: X1 =:= (HX1 :+: TX1),
      ev3: X2 =:= (HX2 :+: TX2),
      ev4: Drop.Aux[X1, Succ[_0], TX1],
      ev5: Drop.Aux[X2, Succ[_0], TX2],
      ev6: Inject[X1, HX1],
      ev7: Inject[X2, HX2]): CCS[A1, (HX1, HX2) :+: TX1 :+: TX2 :+: CNil] =
    choice[A1, X1, X2, HX1, HX2, TX1, TX2](self)(m2)

  // def |[X2, X](
  //     m2: CCS[A1, X2])(implicit
  //     ev0: ClearProduct.Aux[X1, X2, X]): CCS[A1 \/ A1, X] =
  //   parallel(self, m2)

  def \(r: Set[A1]): CCS[A1, X1] = restrict(self)(r)

  // def r[A2, X2](
  //     rs: (A1, A2)*)(implicit
  //     ev0: X1 <-> X2): CCS[A2, X2] =
  //   renaming(self)(rs: _*)(ev0)
}

trait ToCCSOps {

  implicit def toCCSOps[A, X <: Coproduct](m: CCS[A, X]): CCSOps[A, X] =
    new CCSOps[A, X](m)

  implicit class InOutHelper[A](a: A) {
    def in: A \/ A = a.left
    def out: A \/ A = a.right
  }

  implicit class PrefixHelper[A, X](m: => CCS[A, X]) {
    def %:(a: A \/ A): CCS[A, X :+: X :+: CNil] = action[A, X](m)(a)
  }
}
