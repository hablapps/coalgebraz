package org.hablapps.coalgebraz

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._, Scalaz._

object Coalgebraz {

  def always[I, B, X](v: B): Coentity[I, Void, B, X] = { x =>
    Entity(v, _ => (List.empty, Option(x)))
  }

  def until[I, B, X](v: B, f: I => Boolean): Coentity[I, Void, B, X] = { x =>
    Entity(v, i => if (f(i)) (List.empty, None) else (List.empty, Option(x)))
  }

  def constant[A]: Coentity[Void, Void, A, A] =
    s => Entity(s, _ => ??? /* does never happen */)

  def withState[I, O, B, X, X2](
      co: Coentity[I, O, B, X])(implicit
      iso: X <-> X2): Coentity[I, O, B, X2] = { x2 =>
    val Entity(obs, nxt) = co(iso.from(x2))
    Entity(obs, i => nxt(i).map(_ map iso.to))
  }

  def withObservable[I, O, B, X, B2](
      co: Coentity[I, O, B, X])(implicit
      ev0: B -> B2): Coentity[I, O, B2, X] = { x =>
    val Entity(obs, nxt) = co(x)
    Entity(ev0.to(obs), nxt)
  }

  // Feeds a coalgebra with a list of inputs and returns the final state (if
  // any) along with the list of outputs that were generated by the system.
  def feed[I, O, B, X](
      co: Coentity[I, O, B, X],
      in: List[I],
      x: X): (List[O], Option[X]) = in match {
    case Nil => (List.empty, Option(x))
    case i::is => {
      val Entity(_, nxt) = co(x)
      nxt(i) match {
        case (os, Some(x2)) => feed(co, is, x2).mapElements(_1 = os ++ _)
        case (os, None) => (os, None)
      }
    }
  }

  // Adapts a system to map input broader events into smaller ones, given a
  // mapper function.
  def routeIn[I, O, B, X, I2](
      co: Coentity[I, O, B, X])(implicit
      r: Router[B, I2, I]): Coentity[I2, O, B, X] = { x =>
    val Entity(obs, nxt) = co(x)
    Entity(obs, i => feed(co, r(obs)(i), x))
  }

  // Adapts a system to map output smaller events into broader ones, given a
  // mapper function.
  def routeOut[I, O, B, X, O2](
      co: Coentity[I, O, B, X])(implicit
      r: Router[B, O, O2]): Coentity[I, O2, B, X] = { x =>
    val Entity(obs, nxt) = co(x)
    Entity(obs, i => nxt(i).swap.map(_ flatMap r(obs)).swap)
  }

  def routeBack[I, O, B, X](
      co: Coentity[I, O, B, X])(implicit
      r: B => O => List[I]): Coentity[I, O, B, X] = { x =>
    type Out = (List[O], Option[X])

    def g(acc: List[O], q: List[O], s: X, nxt: I => Out): Out = q match {
      case Nil => (acc, Option(s))
      case h::t => {
        r(co(s).observe)(h) match {
          case Nil => g(acc :+ h, t, s, nxt)
          case is => {
            val (os, ox) = feed(co, is, s)
            ox.fold((acc ++ q, Option.empty[X]))(g(acc :+ h, t ++ os, _, nxt))
          }
        }
      }
    }

    val Entity(obs, nxt) = co(x)
    Entity(obs, i => nxt(i) match {
      case res@(_, None) => res
      case (os, Some(x2)) => g(List.empty, os, x2, nxt)
    })
  }

  def outputFromBehaviour[I, O, B, X](
      co: Coentity[I, O, B, X])(
      f: B => List[O]): Coentity[I, O, B, X] = { x =>
    val Entity(obs, nxt) = co(x)
    Entity(obs, i => nxt(i) match {
      case res@(_, None) => res
      case (os, Some(x2)) => {
        val Entity(b, _) = co(x2)
        (os ++ f(b), Option(x2))
      }
    })
  }

  // Turns a single coalgebra with its associated state into a coalgebra which
  // handles a list of such states. The input for the new system is `CoseqIn`, a
  // type of event which lets the programmer to add new elements or alter the
  // states individually. Removal is not covered since we're working with
  // `Entity` which is able to stop by its own.
  //
  // XXX: this implementation is quite ugly, I'm sure it can be cleaned up,
  // though there are other priorities right now.
  def toCoseq[I, O, B, X](
      co: Coentity[I, O, B, X])(implicit
      sq: Sq[List, Option]): CoentitySeq[I, O, B, X] = { xs =>
    val es = xs map (co(_))
    val bs = es map (_.observe)
    val nx = es map (_.next)
    Entity(bs, _ match {
      case Elem(f) => {
        val emp = List.empty[CoseqOut[O, X]]
        val ts: List[(List[CoseqOut[O, X]], Option[X])] =
          zip3(xs, nx, bs map f) map {
            case (x, _, None) => (emp, Option(x))
            case (x, nxt, Some(i)) => {
              ((nxt(i).swap.map { os =>
                List(os.toNel.map(WrappedOut.apply[O, X]))
                  .sequence
                  .getOrElse(emp)
              }).extend {
                case (None, os) => Removed[O, X](x) :: os
                case (_, os)    => os
              }).swap
            }
          }
        ts.unzip.swap.map(_.flatten).swap.map(sq(_))
      }
      case Prepend(x) => (List(Prepended(x)), Option(x :: xs))
    })
  }

  def coexist[I1, I2, I, O1, O2, O, B1, B2, B, X1, X2](
      co1: Coentity[I1, O1, B1, X1],
      co2: Coentity[I2, O2, B2, X2])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[B1, B2, B]): Coentity[I, O, B, (X1, X2)] = { x =>
    val (s, t) = x
    val Entity(obs1, nxt1) = co1(s)
    val Entity(obs2, nxt2) = co2(t)
    Entity(ev2(obs1, obs2), i => ev0(
      i1 => nxt1(i1).bimap(_.map(o => ev1(o.left)), _.map((_, t))),
      i2 => nxt2(i2).bimap(_.map(o => ev1(o.right)), _.map((s, _))), i))
  }

  // Permits two coalgebras to share the very same inner state.
  def fusion[I1, I2, I, O1, O2, O, B1, B2, B, X](
      co1: Coentity[I1, O1, B1, X],
      co2: Coentity[I2, O2, B2, X])(implicit
      ev0: ClearSum.Aux[I1, I2, I],
      ev1: ClearSum.Aux[O1, O2, O],
      ev2: ClearProduct.Aux[B1, B2, B]): Coentity[I, O, B, X] = { x =>
    val Entity(obs1, nxt1) = co1(x)
    val Entity(obs2, nxt2) = co2(x)
    Entity(ev2(obs1, obs2), i => ev0(
      i1 => nxt1(i1).swap.map(os => os.map(o1 => ev1(o1.left))).swap,
      i2 => nxt2(i2).swap.map(os => os.map(o2 => ev1(o2.right))).swap,
      i))
  }

  def flow[A, I, O, B, B1, B2, X, X1, X2](
      co1: Coentity[I, A, B1, X1],
      co2: Coentity[A, O, B2, X2])(implicit
      ev0: ClearProduct.Aux[B1, B2, B],
      ev1: ClearProduct.Aux[X1, X2, X],
      ev2: (X1, X2) <-> X): Coentity[I, O, B, X] = { x =>
    val Entity(obs1, nxt1) = co1(ev2.from(x)._1)
    val x2 = ev2.from(x)._2
    val Entity(obs2, _) = co2(x2)
    Entity(ev0(obs1, obs2), { i1 =>
      val (o1s, ox1) = nxt1(i1)
      val (o2s, ox2) = feed(co2, o1s, x2)
      (o2s, (ox1 |@| ox2)(ev1(_, _)))
    })
  }
}
