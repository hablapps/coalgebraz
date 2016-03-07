package coalgebraz.syntax

import coalgebraz._, Coalgebraz._

class StreamOps[H, X](val self: Stream[H, X]) {

  def merge[Y](s: Stream[H, Y]): Stream[H, (X, Y, Boolean)] =
    mergeS(self, s)

  def until(f: H => Boolean, s: Stream[H, X]): Stream[H, (X, Boolean)] =
    untilS(self, s)(f)
}

trait ToStreamOps {
  implicit def toStreamOps[H, X](s: Stream[H, X]): StreamOps[H, X] =
    new StreamOps(s)
}
