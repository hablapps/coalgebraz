package coalgebraz
package example.basic

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import shapeless._

import Coalgebraz._

object CCSRaw extends App {

  /* Deep Embedding */

  sealed trait Process[A]

  object Process {

    def empty[A] = new Empty[A]()

    // Laziness compliant
    implicit class ProcessHelper[A](p: => Process[A]) {
      def %:(h: A \/ A): Process[A] = Action(h, p)
      def ++(p2: => Process[A]) = Choice(p, p2)
      def \(r: Set[A]): Process[A] = Restrict(p, r)
      def :/(f: PartialFunction[A, A]): Process[A] = Rename(p, f)
      def |(p2: => Process[A]): Process[A] = Parallel(p, p2)
    }
  }

  case class Empty[A]() extends Process[A]

  class Action[A](val h: A \/ A, _t: => Process[A]) extends Process[A] {
    lazy val t: Process[A] = _t
  }

  object Action {
    def apply[A](h: A \/ A, t: => Process[A]): Action[A] = new Action(h, t)
    def unapply[A](a: Action[A]) = Option((a.h, a.t))
  }

  class Choice[A](_p1: => Process[A], _p2: => Process[A]) extends Process[A] {
    lazy val p1: Process[A] = _p1
    lazy val p2: Process[A] = _p2
  }

  object Choice {
    def apply[A](p1: => Process[A], p2: => Process[A]) = new Choice(p1, p2)
    def unapply[A](p: Choice[A]) = Option((p.p1, p.p2))
  }

  class Parallel[A](_p1: => Process[A], _p2: => Process[A]) extends Process[A] {
    lazy val p1: Process[A] = _p1
    lazy val p2: Process[A] = _p2
  }

  object Parallel {
    def apply[A](p1: => Process[A], p2: => Process[A]) = new Parallel(p1, p2)
    def unapply[A](p: Parallel[A]) = Option((p.p1, p.p2))
  }

  class Restrict[A](_p: => Process[A], val r: Set[A]) extends Process[A] {
    lazy val p: Process[A] = _p
  }

  object Restrict {
    def apply[A](p: => Process[A], r: Set[A]) = new Restrict(p, r)
    def unapply[A](p: Restrict[A]) = Option((p.p, p.r))
  }

  class Rename[A](
      _p: => Process[A],
      val f: PartialFunction[A, A]) extends Process[A] {
    lazy val p: Process[A] = _p
  }

  object Rename {
    def apply[A](p: => Process[A], f: PartialFunction[A, A]) = new Rename(p, f)
    def unapply[A](p: Rename[A]) = Option((p.p, p.f))
  }

  /* CCS expressions */

  // The following examples were extracted from these nice lecture notes:
  // http://people.cis.ksu.edu/~schmidt/705a/Lectures/intro2ccs.pdf

  def Z = Process.empty[Channel]

  def CS: Process[Channel] = pub.out %: coin.out %: coffee.in %: CS

  def CM: Process[Channel] = coin.in %: coffee.out %: CM

  def CTM: Process[Channel] =
    coin.in %: (coffee.out %: CTM ++ tea.out %: CTM)

  def SmUni = (CS | CM) \ Set(coin, coffee)

  def VM: Process[Channel] = coin.in %: item.out %: VM

  def CHM = VM :/ { case `item` => chocs }
  def DFM = VM :/ { case `item` => figs }
  def CRM = VM :/ { case `item` => crisps }

  /* Semantics, coalgebraically */

  // In CCS, AST corresponds with state!
  def semantics[A]: CCS[A, Process[A]] = {
    def f(p: Process[A]): LazyList[(A \/ A, Process[A])] =
      p match {
        case Empty() => LazyList.empty
        case Action(h, t) => LazyList((h, t))
        case Choice(p1, p2) => f(p1) ++ f(p2)
        case Parallel(p1, p2) => {

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

          // handshake
          filter2(f(p1), f(p2))((t1, t2) => isDual(t1._1, t2._1))
            .headOption
            .fold(f(p1).map(_.map(_ | p2)) ++ f(p2).map(_.map(p1 | _))) {
              case ((_, x1), (_, x2)) => f(x1 | x2)
            }
        }
        case Restrict(p, r) => f(p)
          .filterNot(_._1.fold(r contains _, r contains _))
          .map(_.map(_ \ r))
        case Rename(p, g) => {
          val h: PartialFunction[A, A] = g orElse PartialFunction(identity[A])
          f(p).map(_.bimap(
            aa => aa.fold(h andThen -\/.apply, h andThen \/-.apply),
            p2 => p2 :/ g))
        }
      }
    ccs(f)
  }

  /* Interpretation in final coalgebra (LTS) */

  runCCSIO(semantics[Channel])(
    DFM,
    l => println(s"⇒ $l".toLowerCase),
    r => println(s"⇒ _${r}_".toLowerCase))

  /* Example Channels (aka. Ports) datatypes */

  sealed trait Channel
  case object pub extends Channel
  case object coin extends Channel
  case object coffee extends Channel
  case object tea extends Channel
  case object chocs extends Channel
  case object figs extends Channel
  case object crisps extends Channel
  case object item extends Channel

  object Channel {
    implicit val readChannel: Read[Channel \/ Channel] =
      new Read[Channel \/ Channel] {
        def read(s: String): Option[Channel \/ Channel] = s match {
          case "pub" => pub.in.some
          case "coin" => coin.in.some
          case "coffee" => coffee.in.some
          case "tea" => tea.in.some
          case "chocs" => chocs.in.some
          case "figs" => figs.in.some
          case "crisps" => crisps.in.some
          case "_pub_" => pub.out.some
          case "_coin_" => coin.out.some
          case "_coffee_" => coffee.out.some
          case "_tea_" => tea.out.some
          case "_chocs_" => chocs.out.some
          case "_figs_" => figs.out.some
          case "_crisps_" => crisps.out.some
          case _ => Option.empty
        }
      }
  }
}
