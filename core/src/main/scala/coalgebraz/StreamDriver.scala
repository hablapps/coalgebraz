package coalgebraz

trait StreamDriver {

  def run[H, X](s: Stream[H, X])(x: X): List[H] = unfold(s, x)

  def runIO[H, X](s: Stream[H, X])(x: X, eff: H => Unit): Unit =
    run(s)(x).foreach(eff)

  def unfold[H, X](s: Stream[H, X], x1: X): List[H] = {
    val StreamF(h, x2) = s(x1)
    h :: unfold(s, x2)
  }
}
