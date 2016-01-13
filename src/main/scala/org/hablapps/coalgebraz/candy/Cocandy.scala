package org.hablapps.coalgebraz.candy

import scala.util.Random

import scalaz._, Scalaz._, Isomorphism.<=>

import org.hablapps.coalgebraz._
import org.hablapps.coalgebraz.Store
import Coalgebraz._, CoentityOps._
import Sq.someOrNone
import Nat.Syntax._
import Isos._

object Cocandy {

  val cokey: Coentity[Void, Void, String, String] = constant[String]

  val coflavour: Coistore[FlavourIn, Flavour, Flavour] =
    s => IStore(s, _ match {
      case Become(flavour) => flavour
    })

  val coposition: Coistore[PositionIn, (Int, Int), (Int, Int)] = {
    case s@(x, y) => IStore(s, _ match {
      case OverX(f) => (f(x), y)
      case OverY(f) => (x, f(y))
    })
  }

  val cocandy1: Coentity[CandyIn1, Void, Candy, Candy] =
    (cokey |+| coflavour |+| coposition)
      .withState[Candy]
      .withObservable[Candy]
      .routeIn(_ => _ match {
        case Fall(n)    => List(OverY(_ + n).right)
        case Slide(dir) => List(dir.toPositionIn.right)
        case Mutate(fl) => List(Become(fl).left)
      })

  val cocandy2: Coentity[CandyIn2, CandyOut, Candy, Candy] = {
    case c: Candy => Entity(c, _ => (List(ByeCandy), None))
  }

  val cocandy: Coentity[CandyIn, CandyOut, Candy, Candy] =
    cocandy1 /+\ cocandy2

  val cocandies: CoentitySeq[CandyIn, CandyOut, Candy, Candy] =
    cocandy.toCoseq

  val cosize: Coentity[Void, Void, Int, Int] = constant[Int]

  // XXX: side-effecting random. It'll be nice to use a pure one!
  val corandom: Coistream[Int, Random] = { rnd =>
    IStream(rnd.nextInt, rnd)
  }

  val coboard: Coentity[BoardIn, BoardOut, Board, Board] = {
    val co = (cosize |+| cocandies)
      .withState[Board]
      .withObservable[Board]
    ???
    (co |+| corandom)
      .withObservable[Board](_._1)
    ???
  }

  def cocounter(limit: Nat): Costore[CounterIn, Nat, Nat] = { x =>
    Store(x, _ match {
      case Increase(n) => if (x + n > limit) None else Option(x + n)
      case Decrease(n) => Option(x - n)
    })
  }
}
