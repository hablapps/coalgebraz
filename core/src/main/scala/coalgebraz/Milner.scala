package coalgebraz

import scalaz._, Scalaz._

trait MilnerCore extends syntax.ToMilnerOps {

  type Milner[A, X] = Coalgebra[MilnerF[A, ?], X]

  def milner[A, X](pi1: X => List[(A \/ A, X)]): Milner[A, X] =
    pi1 andThen MilnerF.apply

  def parallel[A, X1, X2, X](
      m1: Milner[A, X1],
      m2: Milner[A, X2])(implicit
      ev0: ClearProduct.Aux[X1, X2, X]): Milner[A \/ A, X] = { x =>

    def isDual(v1: A \/ A, v2: A \/ A): Boolean = (v1, v2) match {
      case (-\/(a1), \/-(a2)) if (a1 == a2) => true
      case (\/-(a1), -\/(a2)) if (a1 == a2) => true
      case _ => false
    }

    def filter2[A, B](
        l1: List[A], l2: List[B])(
        p: (A, B) => Boolean): List[(A, B)] =
      for {
        a <- l1
        b <- l2
        if p(a, b)
      } yield (a, b)

    def handshake(
        nxt1: List[(A \/ A, X1)],
        nxt2: List[(A \/ A, X2)]): (List[(A \/ A, X1)], List[(A \/ A, X2)]) = {
      filter2(nxt1, nxt2)((tp1, tp2) => isDual(tp1._1, tp2._1)).headOption.fold(
        (nxt1, nxt2)) {
          case ((_, x1), (_, x2)) => handshake(m1(x1).next, m2(x2).next)
        }
    }

    val x1 = ev0.piA(x)
    val x2 = ev0.piB(x)

    val (_nxt1, _nxt2) = handshake(m1(x1).next, m2(x2).next)

    MilnerF(
      _nxt1.map(_.bimap(_.left, ev0(_, x2))) ++
        _nxt2.map(_.bimap(_.right, ev0(x1, _))))
  }
}
