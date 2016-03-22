package coalgebraz
package syntax

import shapeless._

import Coalgebraz._

class StreamOps[H, X](val self: Stream[H, X]) {

  def merge[Y](s: Stream[H, Y]): Stream[H, (X, Y) :+: (Y, X) :+: CNil] =
    mergeS(self)(s)

  def until[Y](
      p: H => Boolean,
      s: Stream[H, Y]): Stream[H, (X, Y) :+: Y :+: CNil] =
    untilS(self)(s, p)

  def odds: Stream[H, X] = oddsS(self)

  def evens: Stream[H, X] = evensS(self)
}

trait ToStreamOps {
  implicit def toStreamOps[H, X](s: Stream[H, X]): StreamOps[H, X] =
    new StreamOps(s)
}
