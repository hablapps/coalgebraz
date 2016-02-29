package coalgebraz

import Coalgebraz._

class MooreOps[I, O, X](val self: Moore[I, O, X]) {

}

trait ToMooreOps {
  implicit def toMooreOps[I, O, X](m: Moore[I, O, X]): MooreOps[I, O, X] =
    new MooreOps[I, O, X](m)
}
