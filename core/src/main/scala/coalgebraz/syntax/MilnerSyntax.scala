package coalgebraz
package syntax

import Coalgebraz._

class MilnerOps[A1, X1](val self: Milner[A1, X1]) {

}

trait ToMilnerOps {
  implicit def toMilnerOps[A, X](m: Milner[A, X]): MilnerOps[A, X] =
    new MilnerOps[A, X](m)
}
