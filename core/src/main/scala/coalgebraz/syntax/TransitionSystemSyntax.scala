package coalgebraz
package syntax

import Coalgebraz._

class TransitionSystemOps[X1](val self: TransitionSystem[X1])

trait ToTransitionSystemOps {
  implicit def toTransitionSystemOps[X](
      ts: TransitionSystem[X]): TransitionSystemOps[X] =
    new TransitionSystemOps[X](ts)
}
