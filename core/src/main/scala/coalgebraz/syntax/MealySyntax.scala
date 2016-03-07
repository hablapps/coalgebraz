package coalgebraz
package syntax

import scalaz._, Scalaz._

import Coalgebraz._

class MealyOps[I1, O1, X1](val self: Mealy[I1, O1, X1]) {

  def in[I2](f: I2 => NonEmptyList[I1]): Mealy[I2, O1, X1] = inMealy(self)(f)

  def out[O2](f: O1 => O2): Mealy[I1, O2, X1] = outMealy(self)(f)

  def |*|[I2, I, O2, O, X2, X](
      m2: Mealy[I2, O2, X2])(implicit
      ev0: ClearProduct.Aux[I1, I2, I],
      ev1: ClearProduct.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[X1, X2, X]): Mealy[I, O, X] =
    coexistMealy(self, m2)(ev0, ev1, ev2)
}

trait ToMealyOps {
  implicit def toMealyOps[I, O, X](m: Mealy[I, O, X]): MealyOps[I, O, X] =
    new MealyOps[I, O, X](m)
}
