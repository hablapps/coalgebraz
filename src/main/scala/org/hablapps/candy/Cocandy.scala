package org.hablapps.candy

import scala.util.Random
import Function.const

import scalaz._, Scalaz._, Isomorphism.<=>

import org.hablapps.coalgebraz._
import org.hablapps.coalgebraz.StoreF
import Coalgebraz._, EntityOps._
import Sq.someOrNone
import Nat.Syntax._
import Isos.{ isoCandy, isoBoard }
import Routing._

object Cocandy {

  val key: Entity[Void, Void, String, String] = blocked[String]

  val flavour: IStore[FlavourIn, Flavour, Flavour] =
    s => IStoreF(s, _ match {
      case Become(flavour) => flavour
    })

  val position: IStore[PositionIn, (Int, Int), (Int, Int)] = {
    case s@(x, y) => IStoreF(s, _ match {
      case OverX(f) => (f(x), y)
      case OverY(f) => (x, f(y))
    })
  }

  val candy1: Entity[CandyIn1, Void, Candy, Candy] =
    (key |*| flavour |*| position)
      .withState[Candy]
      .withObservable[Candy]
      .routeIn[CandyIn1]

  val candy2: Entity[CandyIn2, CandyOut, Candy, Candy] = {
    case c: Candy => EntityF(c, _ => (List(ByeCandy), None))
  }

  // XXX: I'm not a big fan of this implementation, perhaps there's a cleaner
  // way to achieve `candy`.
  val candy: Entity[CandyIn, CandyOut, Candy, Candy] =
    (candy1 \*/ candy2) withObservable (To { case (c1, _) => c1 })

  val candies: EntitySeq[CandyIn, CandyOut, Candy, Candy] =
    candy.toCoseq

  val size: Entity[Void, Void, Int, Int] = blocked[Int]

  // XXX: side-effecting random. It'll be nice to use a pure one!
  val factory: IStream[Candy, Random] = { rnd =>
    IStreamF(intToCandy(rnd.nextInt), rnd)
  }

  val board: Entity[BoardIn, BoardOut, (Board, Candy), (Board, Random)] =
    ((size |*| candies)
      .withState[Board]
      .withObservable[Board]
      |*| factory)
        .routeIn[BoardIn]
        .routeOut[BoardOut]
        .outputFromBehaviour(observeForReaction)
        .routeBack(routeBackBoard)

  def score(limit: Nat): Entity[CounterIn, CounterOut, Nat, Nat] = { x =>
    EntityF(x, _ match {
      case Increase(n) if x + n > limit => (List(Done), None)
      case Increase(n) => (List.empty, Option(x + n))
      case Decrease(n) => (List.empty, Option(x - n))
    })
  }

  def game(target: Nat): Entity[BoardIn, CounterOut, Game, (Board, Random, Nat)] =
    (board.routeOut[CounterIn] |->| score(target))
      .withState[(Board, Random, Nat)]
      .withObservable(To { case ((b, r), n) => (b, n) })
}
