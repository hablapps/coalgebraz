package coalgebraz.driver

import scala.collection.immutable.{ Stream => LazyList }

import coalgebraz._, Coalgebraz._

trait StreamDriver {

  def run[H, X](s: Stream[H, X], b: Int = 50)(x: X): LazyList[H] =
    unfold(s, x).take(b)

  def runIO[H, X](
      s: Stream[H, X],
      b: Int = 50)(
      x: X,
      e: H => Unit): Unit =
    run(s, b)(x).foreach(e)

  def unfold[H, X](s: Stream[H, X], x1: X): LazyList[H] = {
    val StreamF(h, x2) = s(x1)
    h #:: unfold(s, x2)
  }
}
