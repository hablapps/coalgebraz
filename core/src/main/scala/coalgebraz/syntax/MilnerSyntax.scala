package coalgebraz
package syntax

import scalaz._, Scalaz._

import Coalgebraz._

class MilnerOps[A1, X1](val self: Milner[A1, X1]) {

  def |[X2, X](
      m2: Milner[A1, X2])(implicit
      ev0: ClearProduct.Aux[X1, X2, X]): Milner[A1 \/ A1, X] =
    parallel(self, m2)

  def \(a: A1): Milner[A1, X1] = restrict(self)(a)

  def rename[A2, X2](
      rs: (A1, A2)*)(implicit
      ev0: X1 <-> X2): Milner[A2, X2] =
    renaming(self)(rs: _*)(ev0)
}

trait ToMilnerOps {
  implicit def toMilnerOps[A, X](m: Milner[A, X]): MilnerOps[A, X] =
    new MilnerOps[A, X](m)
}
