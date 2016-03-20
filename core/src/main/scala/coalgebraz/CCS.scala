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

  def choice[A, X <: Coproduct, Y <: Coproduct,
    HX, HY, TX <: Coproduct, TY <: Coproduct](
      m1: CCS[A, X])(
      m2: CCS[A, Y])(implicit
      ev0: IsCCons.Aux[X, HX, TX],
      ev1: IsCCons.Aux[Y, HY, TY],
      // XXX: shapeless is not quite sure about next pair, why not? At least I
      // sleep better with them here.
      ev2: X =:= (HX :+: TX),
      ev3: Y =:= (HY :+: TY),
      // XXX: Isn't this redundant by `ev[01]`?
      ev4: Drop.Aux[X, Succ[_0], TX],
      ev5: Drop.Aux[Y, Succ[_0], TY],
      // XXX: redundant by `ev[01]`?
      ev6: Inject[X, HX],
      ev7: Inject[Y, HY]): CCS[A, (HX, HY) :+: TX :+: TY :+: CNil] = {
    type R = (HX, HY) :+: TX :+: TY :+: CNil
    object toCCS extends Poly1 {
      implicit val caseH  = at[(HX, HY)] { case (hx, hy) =>
        CCSF(m1(Coproduct(hx)).map(x => Coproduct[R](x.drop.get)).next ++
          m2(Coproduct(hy)).map(y => Coproduct[R](y.drop.get)).next)
      }
      implicit val caseTX = at[TX] { tx =>
        m1(tx.extendLeft[HX].asInstanceOf[X]).map(x => Coproduct[R](x.drop.get))
      }
      implicit val caseTY = at[TY] {  ty =>
        m2(ty.extendLeft[HY].asInstanceOf[Y]).map(y => Coproduct[R](y.drop.get))
      }
    }
    _.fold(toCCS)
  }

  // def parallel[A, X1, X2, X](
  //     m1: CCS[A, X1],
  //     m2: CCS[A, X2])(implicit
  //     ev0: ClearProduct.Aux[X1, X2, X]): CCS[A \/ A, X] = { x =>
  //
  //   def filter2[A, B](
  //       l1: LazyList[A], l2: LazyList[B])(
  //       p: (A, B) => Boolean): LazyList[(A, B)] =
  //     for {
  //       a <- l1
  //       b <- l2
  //       if p(a, b)
  //     } yield (a, b)
  //
  //   def isDual(v1: A \/ A, v2: A \/ A): Boolean = (v1, v2) match {
  //     case (-\/(a1), \/-(a2)) if (a1 == a2) => true
  //     case (\/-(a1), -\/(a2)) if (a1 == a2) => true
  //     case _ => false
  //   }
  //
  //   def handshake(
  //       nxt1: LazyList[(A \/ A, X1)],
  //       nxt2: LazyList[(A \/ A, X2)])
  //       : (LazyList[(A \/ A, X1)], LazyList[(A \/ A, X2)]) = {
  //     filter2(nxt1, nxt2)((tp1, tp2) => isDual(tp1._1, tp2._1)).headOption.fold(
  //       (nxt1, nxt2)) {
  //         case ((_, x1), (_, x2)) => handshake(m1(x1).next, m2(x2).next)
  //       }
  //   }
  //
  //   val x1 = ev0.piA(x)
  //   val x2 = ev0.piB(x)
  //
  //   val (_nxt1, _nxt2) = handshake(m1(x1).next, m2(x2).next)
  //
  //   CCSF(
  //     _nxt1.map(_.bimap(_.fold(_.left.left, _.left.right), ev0(_, x2))) ++
  //       _nxt2.map(_.bimap(_.fold(_.right.left, _.right.right), ev0(x1, _))))
  // }
  //
  // def restrict[A, X](m: CCS[A, X])(act: A): CCS[A, X] =
  //   ccs(m(_).next.filter(ax => ax._1.fold(_ != act, _ != act)))
  //
  // def renaming[A1, A2, X1, X2](
  //     m: CCS[A1, X1])(
  //     rs: (A1, A2)*)(implicit
  //     ev0: X1 <-> X2): CCS[A2, X2] = {
  //   val pf: PartialFunction[A1 \/ A1, A2 \/ A2] = rs
  //     .map { case (a1, a2) => {
  //       case -\/(`a1`) => -\/(a2)
  //       case \/-(`a1`) => \/-(a2)
  //     }: PartialFunction[A1 \/ A1, A2 \/ A2] }
  //     .foldRight(PartialFunction.empty[A1 \/ A1, A2 \/ A2])(_ orElse _)
  //   ccs(x2 => m(ev0.from(x2)).next.map(_.bimap(pf, ev0.to)))
  // }
}
