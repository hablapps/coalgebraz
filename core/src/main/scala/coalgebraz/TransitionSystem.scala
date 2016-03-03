package coalgebraz

trait TransitionSystemCore extends syntax.ToTransitionSystemOps {
  type TransitionSystem[X] = Coalgebra[TransitionSystemF[?], X]
}
