package coalgebraz

import scalaz._, Scalaz._

trait StreamCore extends ToStreamOps {

  def stream[H, X](pi1: X => H, pi2: X => X): Stream[H, X] =
    x => StreamF(pi1(x), pi2(x))

  def merge[H, X, Y](
      s1: Stream[H, X],
      s2: Stream[H, Y]): Stream[H, (X, Y, Boolean)] = {
    // TODO: modularise this code!!!
    case (x, y, true) => {
      val StreamF(h, x2) = s1(x)
      StreamF(h, (x2, y, false))
    }
    case (x, y, false) => {
      val StreamF(h, y2) = s2(y)
      StreamF(h, (x, y2, true))
    }
  }
}
