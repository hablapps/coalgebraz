package coalgebraz

import Function.const

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import shapeless._, shapeless.{ :+:, Coproduct }
import ops.coproduct._, ops.coproduct.Inject

import Coalgebraz._

trait CCSCore extends syntax.ToCCSOps {

  type CCS[A, X] = Coalgebra[CCSF[A, ?], X]

  def ccs[A, X](pi1: X => LazyList[(A \/ A, X)]): CCS[A, X] = x => CCSF(pi1(x))

  def empty[A, X]: CCS[A, X] = ccs(const(LazyList.empty))

  def action[A, X](
      m: => CCS[A, X])(
      a: A \/ A): CCS[A, X :+: X :+: CNil] = ccs { xx =>
    object toNext extends Poly1 {
      implicit val caseX = at[X] { x =>
        xx.head.isDefined.fold(
          LazyList(a -> Coproduct[X :+: CNil](x).extendLeft[X]),
          m(x).map(Coproduct[X :+: CNil](_).extendLeft[X]).next)
      }
    }
    xx.fold(toNext)
  }

  def choice[A, X, Y](
      m1: => CCS[A, X])(
      m2: => CCS[A, Y]): CCS[A, (X, Y) :+: X :+: Y :+: CNil] = {
    type R = (X, Y) :+: X :+: Y :+: CNil
    object toNext extends Poly1 {
      implicit val caseXY = at[(X, Y)] { case (x, y) => 
        m1(x).map(x2 => Coproduct[R](x2)).next ++
          m2(y).map(y2 => Coproduct[R](y2)).next
      }
      implicit val caseX  = at[X] { m1(_).map(Coproduct[R](_)).next }
      implicit val caseY  = at[Y] { m2(_).map(Coproduct[R](_)).next }
    }
    ccs(_.fold(toNext))
  }

  def renaming[A, X](
      m: => CCS[A, X])(
      f: A \/ A => A \/ A): CCS[A, X] =
    ccs(m(_).next.map(_.swap.map(f).swap))

  def restrict[A, X](m: CCS[A, X])(r: Set[A]): CCS[A, X] =
    ccs(m(_).next.filterNot(_._1.fold(r.contains, r.contains)))

  def parallel[A, X, Y](
      m1: => CCS[A, X])(
      m2: => CCS[A, Y]): CCS[A, X :: Y :: HNil] = { xy =>

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
  
    def handshake(x: X, y: Y)
        : (X, Y, LazyList[(A \/ A, X)], LazyList[(A \/ A, Y)]) = {
      val nxt1 = m1(x).next
      val nxt2 = m2(y).next
      filter2(nxt1, nxt2)((tp1, tp2) => isDual(tp1._1, tp2._1))
        .headOption.fold((x, y, nxt1, nxt2)) {
          case ((_, x2), (_, y2)) => handshake(x2, y2)
        }
    }
  
    val (x, y, _nxt1, _nxt2) = handshake(xy.head, xy.tail.head)
  
    CCSF(_nxt1.map(_.map(_ :: y :: HNil)) ++ _nxt2.map(_.map(x :: _ :: HNil)))
  }
}
