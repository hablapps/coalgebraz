package coalgebraz

import Function.const

import scalaz._, Scalaz._

trait StreamCore extends ToStreamOps {

  def stream[H, X](pi1: X => H, pi2: X => X): Stream[H, X] =
    x => StreamF(pi1(x), pi2(x))

  def repeatS[H, X](h: H): Stream[H, X] =
    stream(const(h), identity)

  def mergeS[H, X, Y](
      s1: Stream[H, X],
      s2: Stream[H, Y]): Stream[H, (X, Y, Boolean)] = {
    case (x, y, b) => {
      lazy val StreamF(h1, x1) = s1(x)
      lazy val StreamF(h2, y2) = s2(y)
      b.fold(
        StreamF(h1, (x1, y, false)),
        StreamF(h2, (x, y2, true)))
    }
  }

  def untilS[H, X](
      s1: Stream[H, X],
      s2: Stream[H, X])(p: H => Boolean): Stream[H, (X, Boolean)] = {
    case (x, b) => {
      lazy val StreamF(h1, x1) = s1(x)
      lazy val StreamF(h2, x2) = s2(x)
      b.fold(StreamF(h2, (x2, true)), StreamF(h1, (x1, p(h1))))
    }
  }
}
