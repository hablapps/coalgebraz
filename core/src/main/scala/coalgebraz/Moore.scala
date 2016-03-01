package coalgebraz

trait MooreCore extends ToMooreOps {

  type Moore[I, O, X] = Coalgebra[MooreF[I, O, ?], X]

  def moore[I, O, X](pi1: X => O, pi2: X => I => X): Moore[I, O, X] =
    x => MooreF(pi1(x), pi2(x))

  def coexistMoore[I1, I2, I, O1, O2, O, X1, X2, X](
      m1: Moore[I1, O1, X1],
      m2: Moore[I2, O2, X2])(
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearProduct.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[X1, X2, X]): Moore[I, O, X] = { x =>
    val (s, t) = (ev2.piA(x), ev2.piB(x))
    val MooreF(o1, nxt1) = m1(s)
    val MooreF(o2, nxt2) = m2(t)
    MooreF(ev1(o1, o2), i => ev0(
      i1 => ev2(nxt1(i1), t),
      i2 => ev2(s, nxt2(i2)),
      i))
  }

  def feedMoore[I, O, X](m: Moore[I, O, X], in: List[I], x: X): X =
    in.reverse.foldRight(x)((i, x2) => m(x2).next(i))

  def inMoore[I, O, X, I2](
      m: Moore[I, O, X])(
      f: I2 => List[I]): Moore[I2, O, X] =
    moore(x => m(x).output, x => i2 => feedMoore(m, f(i2), x))

  def outMoore[I, O, X, O2](
      m: Moore[I, O, X])(
      f: O => O2): Moore[I, O2, X] =
    moore(x => f(m(x).output), x => m(x).next)
}
