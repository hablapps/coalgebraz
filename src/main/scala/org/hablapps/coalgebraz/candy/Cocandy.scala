package org.hablapps.coalgebraz.candy

import scala.util.Random
import Function.const

import scalaz._, Scalaz._, Isomorphism.<=>

import org.hablapps.coalgebraz._
import org.hablapps.coalgebraz.Store
import Coalgebraz._, CoentityOps._
import Sq.someOrNone
import Nat.Syntax._
import Isos.{ isoCandy, isoBoard }
import Routing._

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
      .routeIn[CandyIn1]

  val cocandy2: Coentity[CandyIn2, CandyOut, Candy, Candy] = {
    case c: Candy => Entity(c, _ => (List(ByeCandy), None))
  }

  // XXX: I'm not a big fan of this implementation, perhaps there's a cleaner
  // way to achieve `cocandy`.
  val cocandy: Coentity[CandyIn, CandyOut, Candy, Candy] =
    cocandy1 /+\ cocandy2

  val cocandies: CoentitySeq[CandyIn, CandyOut, Candy, Candy] =
    cocandy.toCoseq

  val cosize: Coentity[Void, Void, Int, Int] = constant[Int]

  // XXX: side-effecting random. It'll be nice to use a pure one!
  val cofactory: Coistream[Candy, Random] = { rnd =>
    IStream(intToCandy(rnd.nextInt), rnd)
  }

  val coboard: Coentity[BoardIn, BoardOut, (Board, Candy), (Board, Random)] =
    ((cosize |+| cocandies)
      .withState[Board]
      .withObservable[Board]
      |+| cofactory)
        .routeIn[BoardIn]
        .routeOut[BoardOut]
        .outputFromBehaviour(observeForReaction)
        .routeBack(routeBackBoard)

  def coscore(limit: Nat): Coentity[CounterIn, CounterOut, Nat, Nat] = { x =>
    Entity(x, _ match {
      case Increase(n) if x + n > limit => (List(Done), None)
      case Increase(n) => (List.empty, Option(x + n))
      case Decrease(n) => (List.empty, Option(x - n))
    })
  }

  def cogame(target: Nat): Coentity[BoardIn, CounterOut, Game, (Board, Random, Nat)] =
    (coboard.routeOut[CounterIn](routeOutBoard2) |->| coscore(target))
      .withState[(Board, Random, Nat)]
      .withObservable(To { case ((b, r), n) => (b, n) })
}
