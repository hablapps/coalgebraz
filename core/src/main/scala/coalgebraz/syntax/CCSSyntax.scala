package coalgebraz
package syntax

import scalaz._, Scalaz._

import Coalgebraz._

class CCSOps[A1, X1](val self: CCS[A1, X1]) {

  def +(m2: CCS[A1, X1]): CCS[A1, X1] = choice(self)(m2)

  def |[X2, X](
      m2: CCS[A1, X2])(implicit
      ev0: ClearProduct.Aux[X1, X2, X]): CCS[A1 \/ A1, X] =
    parallel(self, m2)

  def \(a: A1): CCS[A1, X1] = restrict(self)(a)

  def r[A2, X2](
      rs: (A1, A2)*)(implicit
      ev0: X1 <-> X2): CCS[A2, X2] =
    renaming(self)(rs: _*)(ev0)
}

trait ToCCSOps {
  implicit def toCCSOps[A, X](m: CCS[A, X]): CCSOps[A, X] =
    new CCSOps[A, X](m)
}
