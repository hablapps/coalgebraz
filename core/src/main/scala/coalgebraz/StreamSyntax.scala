package coalgebraz

import Coalgebraz._

class StreamOps[H, X](val self: Stream[H, X]) {

  def merge[Y](s: Stream[H, Y]): Stream[H, (X, Y, Boolean)] =
    mergeS(self, s)
}

trait ToStreamOps {
  implicit def toStreamOps[H, X](s: Stream[H, X]): StreamOps[H, X] =
    new StreamOps(s)
}
