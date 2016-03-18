package coalgebraz

import Function.const

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import shapeless._, shapeless.{ :+:, Coproduct }, ops.coproduct._
import ops.coproduct.Inject

import Coalgebraz._

trait CCSCore extends CCSDSL with syntax.ToCCSOps {

  type CCS[A, X] = Coalgebra[CCSF[A, ?], X]

  def ccs[A, X](pi1: X => LazyList[(A \/ A, X)]): CCS[A, X] = x => CCSF(pi1(x))

  def empty[A, X]: CCS[A, X] = ccs(const(LazyList.empty))

  def empty2[A]: CCS[A, HNil] = ccs(const(LazyList.empty))

  def empty3[A]: CCS[A, Unit :+: CNil] = ccs(const(LazyList.empty))

  def action[A, X](m: => CCS[A, X])(a: A \/ A): CCS[A, X \/ X] =
    ccs(_.fold(
      x => LazyList(a -> x.right),
      x => m(x).next.map(_.map(_.right))))

  def action2[A, X <: HList](
      m: => CCS[A, X])(a: A \/ A): CCS[A, (Unit \/ Unit) :: X] =
    ccs(hl => hl.head.fold(
      _ => LazyList((a, ().right :: hl.tail)),
      _ => m(hl.tail).next.map(_.map(().right :: _))))

  // def action3[A, A2 <: A, X <: HList, X2 <: HList](
  //     m: => CCS[A, X])(implicit
  //     ev0: Prepend.Aux[(A2 \/ A2) :: HNil, X, X2],
  //     ev1: IsHCons[X2]): CCS[A, X2 \/ X] =
  //   CCS(_.fold(
  //     x2 => LazyList((
  //       x2.head.asInstanceOf[A2 \/ A2],
  //       x2.tail.asInstanceOf[X].right)),
  //     x => m(x).next.map(_.map(_.right))))

  def action4[A, A2 <: A, X <: Coproduct](
      m: => CCS[A, X]): CCS[A, ((A2 \/ A2), X) :+: X :+: CNil] =
    ccs(x => x.select[((A2 \/ A2), X)].fold(
      x.select[X].map(x2 => m(x2).next.map(_.map(
        x3 => Coproduct[((A2 \/ A2), X) :+: X :+: CNil](x3))))
          .getOrElse(LazyList.empty))(
      a2x => LazyList(
        a2x._1 -> Coproduct[((A2 \/ A2), X) :+: X :+: CNil](a2x._2))
    ))

  // XXX: what's the way to use `fold` (How are `Poly`s implemented)? Seems like
  // a nice candidate for this task!
  //
  // XXX: Why isn't `IsCCons` inferring `Inject`? If the head of X is Y, I find
  // it reasonable to inject an `Y` in `X`!!!
  def action5[A, A2 <: A, X <: Coproduct, Y](
      m: => CCS[A, X])(implicit
      ev0: ops.coproduct.Inject[X, Y],
      ev1: IsCCons.Aux[X, Y, _]): CCS[A, (A2 \/ A2, Y) :+: X] = {
    ccs(x => x.select[(A2 \/ A2, Y)].fold(
      x.drop(1).map(x2 => m(x2).next.map(_.map(_.extendLeft[(A2 \/ A2, Y)])))
        .getOrElse(LazyList.empty))(a2x =>
          LazyList(a2x._1 -> Coproduct[X](a2x._2).extendLeft[(A2 \/ A2, Y)])))
  }

  // 6th version! to infinity and even further!
  def action6[A, X <: Coproduct, Y](
      m: => CCS[A, X])(
      a: A \/ A)(implicit
      ev0: IsCCons.Aux[X, Y, _],
      ev1: ops.coproduct.Inject[X, Y]): CCS[A, Y :+: X] =
    ccs(x => x.select[Y].fold(
      x.drop(1).map(x2 => m(x2).next.map(_.map(_.extendLeft[Y])))
        .getOrElse(LazyList.empty))(y =>
          LazyList(a -> Coproduct[X](y).extendLeft[Y])))

  def choice[A, X](m1: CCS[A, X])(m2: CCS[A, X]): CCS[A, X] =
    ccs(x => (m1(x).next ++ m2(x).next).distinct)

  def choice2[A, X, Y](
      m1: CCS[A, X])(
      m2: CCS[A, Y]): CCS[A, X :+: Y :+: CNil] = {
    def f[Z: Inject[X :+: Y :+: CNil, ?]](m: CCS[A, Z])(z: Z) =
      m(z).next.map(_.map(Coproduct(_)))
    object toNext extends Poly1 {
      implicit val caseX = at[X] { f(m1)(_) }
      implicit val caseY = at[Y] { f(m2)(_) }
    }
    ccs(_.fold(toNext))
  }

  def choice3[A, X <: Coproduct, Y <: Coproduct,
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
    type Res = (HX, HY) :+: TX :+: TY :+: CNil
    object toNext extends Poly1 {
      implicit val caseH  = at[(HX, HY)] { case (hx, hy) =>
        m1(Coproduct[X](hx)).next.map(_.map(x => Coproduct[Res](x.drop.get))) ++
          m2(Coproduct[Y](hy)).next.map(_.map(y => Coproduct[Res](y.drop.get)))
      }
      implicit val caseTX = at[TX] { tx =>
        m1(tx.extendLeft[HX].asInstanceOf[X])
          .next.map(_.map(x => Coproduct[Res](x.drop.get)))
      }
      implicit val caseTY = at[TY] {  ty =>
        m2(ty.extendLeft[HY].asInstanceOf[Y])
          .next.map(_.map(y => Coproduct[Res](y.drop.get)))
      }
    }
    ccs(_.fold(toNext))
  }

  def parallel[A, X1, X2, X](
      m1: CCS[A, X1],
      m2: CCS[A, X2])(implicit
      ev0: ClearProduct.Aux[X1, X2, X]): CCS[A \/ A, X] = { x =>

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

    CCSF(
      _nxt1.map(_.bimap(_.fold(_.left.left, _.left.right), ev0(_, x2))) ++
        _nxt2.map(_.bimap(_.fold(_.right.left, _.right.right), ev0(x1, _))))
  }

  def restrict[A, X](m: CCS[A, X])(act: A): CCS[A, X] =
    ccs(m(_).next.filter(ax => ax._1.fold(_ != act, _ != act)))

  def renaming[A1, A2, X1, X2](
      m: CCS[A1, X1])(
      rs: (A1, A2)*)(implicit
      ev0: X1 <-> X2): CCS[A2, X2] = {
    val pf: PartialFunction[A1 \/ A1, A2 \/ A2] = rs
      .map { case (a1, a2) => {
        case -\/(`a1`) => -\/(a2)
        case \/-(`a1`) => \/-(a2)
      }: PartialFunction[A1 \/ A1, A2 \/ A2] }
      .foldRight(PartialFunction.empty[A1 \/ A1, A2 \/ A2])(_ orElse _)
    ccs(x2 => m(ev0.from(x2)).next.map(_.bimap(pf, ev0.to)))
  }
}

trait CCSDSL {

  implicit class InOutHelper[A](a: A) {
    def in: A \/ A = a.left
    def out: A \/ A = a.right
  }

  def prefix[A, X](ax: (A \/ A, X)): LazyList[(A \/ A, X)] =
    LazyList(ax)

  implicit class RenameHelper[A](a: A) {
    def /[B](b: B): (B, A) = (b, a)
  }

  // implicit class PrefixHelper[A, X](m: => CCS[A, X]) {
  //   def %:(a: A \/ A): CCS[A, X \/ X] = action(m)(a)
  // implicit class PrefixHelper2[A, X <: HList, X2 <: HList](m: => CCS[A, X]) {
  //   def %:[A2 <: A, X2 <: HList](
  //       a: A2 \/ A2): CCS[A, ((A2 \/ A2) :: X) \/ X] =
  //     action3[A, A2, X, (A2 \/ A2) :: X](m)
  // }

  // implicit class PrefixHelper3[A, X1 <: HList, X2 <: HList](
  //     m: => CCS[A, X1 \/ X2]) {
  //   def %:[A2 <: A](a: A2 \/ A2): CCS[A, ] = ???
  // }

  // implicit class PrefixHelper[A, X <: Coproduct](m: => CCS[A, X]) {
  //   def %:[A2 <: A](a: A2 \/ A2): CCS[A, ((A2 \/ A2), X) :+: X :+: CNil] =
  //     action4[A, A2, X](m)
  // }

  // XXX: it would be nice to have this inferring the concrete `-\/` or `\/-` to
  // be more strict with the provided execution state.
  implicit class PrefixHelper[A, X <: Coproduct, Y](
      m: => CCS[A, X])(implicit
      ev0: IsCCons.Aux[X, Y, _],
      ev1: ops.coproduct.Inject[X, Y]) {
    def %:(a: A \/ A): CCS[A, Y :+: X] = action6[A, X, Y](m)(a)
  }
}
