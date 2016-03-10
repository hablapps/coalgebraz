package coalgebraz

import Function.const

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import Coalgebraz._

trait MilnerCore extends MilnerDSL with syntax.ToMilnerOps {

  type Milner[A, X] = Coalgebra[MilnerF[A, ?], X]

  def milner[A, X](pi1: X => LazyList[(A \/ A, X)]): Milner[A, X] =
    x => MilnerF(pi1(x))

  def empty[A, X]: Milner[A, X] = milner(const(LazyList.empty))

  // XXX: requires OrderedSyntax to be more usable!
  trait Ordered[A] {
    def compare(a1: A, a2: A): Int
    def min: A
    def max: A
    def lt(a1: A, a2: A): Boolean = compare(a1, a2) < 0
    def let(a1: A, a2: A): Boolean = compare(a1, a2) <= 0
    def gt(a1: A, a2: A): Boolean = compare(a1, a2) > 0
    def get(a1: A, a2: A): Boolean = compare(a1, a2) >= 0
  }

  object Ordered {
    def apply[A](implicit ev: Ordered[A]): Ordered[A] = ev
  }

  def action[A, X: Ordered](
      m: => Milner[A, X])(
      ax: (A \/ A, X)): Milner[A, X] =
    milner { x0 =>
      val x = if (x0 == Ordered[X].max) Ordered[X].min else x0
      if (Ordered[X].get(x, ax._2)) m(x).next else LazyList(ax)
    }

  def choice[A, X](m1: Milner[A, X])(m2: Milner[A, X]): Milner[A, X] =
    milner(x => m1(x).next ++ m2(x).next)

  def parallel[A, X1, X2, X](
      m1: Milner[A, X1],
      m2: Milner[A, X2])(implicit
      ev0: ClearProduct.Aux[X1, X2, X]): Milner[A \/ A, X] = { x =>

    def filter2[A, B](
        l1: LazyList[A], l2: LazyList[B])(
        p: (A, B) => Boolean): LazyList[(A, B)] =
      for {
        a <- l1
        b <- l2
        if p(a, b)
      } yield (a, b)

    def isDual(v1: A \/ A, v2: A \/ A): Boolean = (v1, v2) match {
      case (-\/(a1), \/-(a2)) if (a1 == a2) => true
      case (\/-(a1), -\/(a2)) if (a1 == a2) => true
      case _ => false
    }

    def handshake(
        nxt1: LazyList[(A \/ A, X1)],
        nxt2: LazyList[(A \/ A, X2)])
        : (LazyList[(A \/ A, X1)], LazyList[(A \/ A, X2)]) = {
      filter2(nxt1, nxt2)((tp1, tp2) => isDual(tp1._1, tp2._1)).headOption.fold(
        (nxt1, nxt2)) {
          case ((_, x1), (_, x2)) => handshake(m1(x1).next, m2(x2).next)
        }
    }

    val x1 = ev0.piA(x)
    val x2 = ev0.piB(x)

    val (_nxt1, _nxt2) = handshake(m1(x1).next, m2(x2).next)

    MilnerF(
      _nxt1.map(_.bimap(_.fold(_.left.left, _.left.right), ev0(_, x2))) ++
        _nxt2.map(_.bimap(_.fold(_.right.left, _.right.right), ev0(x1, _))))
  }

  def restrict[A, X](m: Milner[A, X])(act: A): Milner[A, X] = milner(
    m(_).next.filter(ax => ax._1.fold(_ != act, _ != act)))

  def renaming[A1, A2, X1, X2](
      m: Milner[A1, X1])(
      rs: (A1, A2)*)(implicit
      ev0: X1 <-> X2): Milner[A2, X2] = {
    val pf: PartialFunction[A1 \/ A1, A2 \/ A2] = rs
      .map { case (a1, a2) => {
        case -\/(`a1`) => -\/(a2)
        case \/-(`a1`) => \/-(a2)
      }: PartialFunction[A1 \/ A1, A2 \/ A2] }
      .foldRight(PartialFunction.empty[A1 \/ A1, A2 \/ A2])(_ orElse _)
    milner(x2 => m(ev0.from(x2)).next.map(_.bimap(pf, ev0.to)))
  }
}

trait MilnerDSL {

  implicit class InOutHelper[A](a: A) {
    def in: A \/ A = a.left
    def out: A \/ A = a.right
  }

  def prefix[A, X](ax: (A \/ A, X)): LazyList[(A \/ A, X)] =
    LazyList(ax)

  implicit class RenameHelper[A](a: A) {
    def /[B](b: B): (B, A) = (b, a)
  }

  implicit class PrefixHelper[A, X: Ordered](m: => Milner[A, X]) {
    def %:(aax: (A \/ A, X)): Milner[A, X] = action(m)(aax)
  }
}
