package coalgebraz
package syntax

import Coalgebraz._

class MooreOps[I1, O1, X1](val self: Moore[I1, O1, X1]) {

  def in[I2](f: I2 => List[I1]): Moore[I2, O1, X1] = inMoore(self)(f)

  def out[O2](f: O1 => O2): Moore[I1, O2, X1] = outMoore(self)(f)

  def |*|[I2, I, O2, O, X2, X](
      m2: Moore[I2, O2, X2])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearProduct.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[X1, X2, X]): Moore[I, O, X] =
    coexistMoore(self, m2)(ev0, ev1, ev2)
}

trait ToMooreOps {
  implicit def toMooreOps[I, O, X](m: Moore[I, O, X]): MooreOps[I, O, X] =
    new MooreOps[I, O, X](m)
}
