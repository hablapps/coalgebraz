package coalgebraz

import Coalgebraz._

trait TransitionSystemCore extends syntax.ToTransitionSystemOps {

  type TransitionSystem[X] = Coalgebra[TransitionSystemF[?], X]

  def transition[X](f: X => List[X]): TransitionSystem[X] =
    f andThen TransitionSystemF.apply
}
