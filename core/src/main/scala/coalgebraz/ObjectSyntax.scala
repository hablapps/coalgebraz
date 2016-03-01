package coalgebraz

import Coalgebraz._

class ObjectOps[I1, O1, E1, X1](val self: Object[I1, O1, E1, X1]) {

}

trait ToObjectOps {
  implicit def toObjectOps[I, O, E, X](
      obj: Object[I, O, E, X]): ObjectOps[I, O, E, X] = 
    new ObjectOps[I, O, E, X](obj)
}
