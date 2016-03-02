package coalgebraz

import Coalgebraz._

class ObjectOps[I1, O1, E1, X1](val self: Object[I1, O1, E1, X1]) {

  def |=>|[O, X2, X](
      ob: Object[O1, O, E1, X2])(implicit
      ev0: ClearProduct.Aux[X1, X2, X]): Object[I1, O, E1, X] =
    andThenObject(self, ob)
}

trait ToObjectOps {
  implicit def toObjectOps[I, O, E, X](
      ob: Object[I, O, E, X]): ObjectOps[I, O, E, X] =
    new ObjectOps[I, O, E, X](ob)
}
