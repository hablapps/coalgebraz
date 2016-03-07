package coalgebraz

trait MilnerCore {

  type Milner[A, X] = Coalgebra[MilnerF[A, ?], X]

  def milner[A, X](pi1: X => List[(A, X)]): Milner[A, X] =
    pi1 andThen MilnerF.apply
}
