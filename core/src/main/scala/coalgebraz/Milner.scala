package coalgebraz

trait MilnerCore {

  type Milner[A, X] = Coalgebra[MilnerF[A, ?], X]
}
