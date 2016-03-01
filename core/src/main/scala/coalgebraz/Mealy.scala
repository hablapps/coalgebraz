package coalgebraz

import scalaz._, Scalaz._

trait MealyCore extends ToMealyOps {

  type Mealy[I, O, X] = Coalgebra[MealyF[I, O, ?], X]

  def mealy[I, O, X](pi1: X => I => (O, X)): Mealy[I, O, X] =
    x => MealyF(pi1(x))

  // XXX: can't receive ev0 as sum evicende, an output is needed!
  def coexistMealy[I1, I2, I, O1, O2, O, X1, X2, X](
      m1: Mealy[I1, O1, X1],
      m2: Mealy[I2, O2, X2])(
      ev0: ClearProduct.Aux[I1, I2, I],
      ev1: ClearProduct.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[X1, X2, X]): Mealy[I, O, X] = { x =>
    val (s, t) = (ev2.piA(x), ev2.piB(x))
    val MealyF(nxt1) = m1(s)
    val MealyF(nxt2) = m2(t)
    MealyF { i =>
      val (i1, i2) = (ev0.piA(i), ev0.piB(i))
      val (o1, x1) = nxt1(i1)
      val (o2, x2) = nxt2(i2)
      (ev1(o1, o2), ev2(x1, x2))
    }
  }

  def foldNel[A, B](
      nel: NonEmptyList[A])(
      just: A => B)(
      cons: (A, B) => B): B = nel match {
    case NonEmptyList(x, INil()) => just(x)
    case NonEmptyList(x1, ICons(x2, xs)) =>
      cons(x1, foldNel(NonEmptyList(x2, xs.toList: _*))(just)(cons))
  }

  def feedMealy[I, O, X](m: Mealy[I, O, X], in: NonEmptyList[I], x: X): (O, X) =
    foldNel(in.reverse)(i => m(x).next(i))((i, ox) => m(ox._2).next(i))

  def inMealy[I, O, X, I2](
      m: Mealy[I, O, X])(
      f: I2 => NonEmptyList[I]): Mealy[I2, O, X] =
    mealy(x => i => feedMealy(m, f(i), x))

  def outMealy[I, O, X, O2](
      m: Mealy[I, O, X])(
      f: O => O2): Mealy[I, O2, X] =
    mealy(x => m(x).next andThen (_.swap.map(f).swap))
}
