package coalgebraz

import scalaz._, Scalaz._

trait ObjectCore extends ToObjectOps {

  type Object[I, O, E, X] = Coalgebra[ObjectF[I, O, E, ?], X]

  // XXX: `object` seems to be used somewhere else :)
  def obj[I, O, E, X](method: X => I => E \/ (O, X)): Object[I, O, E, X] =
    method andThen ObjectF.apply

  def afterObject[I, OI, O, E, X1, X2, X](
      ob1: Object[I, OI, E, X1],
      ob2: Object[OI, O, E, X2])(implicit
      ev0: ClearProduct.Aux[X1, X2, X]): Object[I, O, E, X] = { x =>
    val (s, t) = (ev0.piA(x), ev0.piB(x))
    ObjectF(i => ob1(s).method(i).fold(
      e1 => e1.left[(O, X)],
      { case (o1, x1) => ob2(t).method(o1).fold(
        e2 => e2.left,
        ox => ox.map(x2 => ev0(x1, x2)).right)
      }))
  }
}
