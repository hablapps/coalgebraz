package coalgebraz

trait TransitionSystemCore {
  type TransitionSystem[X] = Coalgebra[TransitionSystemF[?], X]
}
