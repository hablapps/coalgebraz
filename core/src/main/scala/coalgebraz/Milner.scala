package coalgebraz

import scalaz._, Scalaz._

trait MilnerCore extends syntax.ToMilnerOps {

  type Milner[A, X] = Coalgebra[MilnerF[A, ?], X]

  def milner[A, X](pi1: X => List[(A \/ A, X)]): Milner[A, X] =
    pi1 andThen MilnerF.apply

  def parallel[A, X1, X2, X](
      m1: Milner[A, X1],
      m2: Milner[A, X2])(implicit
      ev0: ClearProduct.Aux[X1, X2, X]): Milner[A, X] =
    ???
}
