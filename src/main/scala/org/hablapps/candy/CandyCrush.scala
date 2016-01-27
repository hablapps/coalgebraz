package org.hablapps.candy

import scala.util.Random
import Function.const

import scalaz._, Scalaz._, Isomorphism.<=>

import org.hablapps.coalgebraz._
import org.hablapps.coalgebraz.StoreF
import Coalgebraz._, EntityOps._
import Sq.someOrNone
import Nat.Syntax._
import Isos.{ isoCandy, isoBoard }, To.eqTo
import Routing._

object CandyCrush {

  val key: Entity[Void, Void, String, String] = blocked(eqTo)

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

  val candy: Entity[CandyIn, CandyOut, Candy, Candy] =
    ((key |*| flavour |*| position)
      .carrier[Candy]
      .observe[Candy]
      .in[CandyIn]
      .out[CandyOut]) |~| (_ == Crush, _ => _ => List(ByeCandy))

  val candies: EntitySeq[CandyIn, CandyOut, Candy, Candy] =
    candy.toCoseq

  val size: Entity[Void, Void, Int, Int] = blocked(eqTo)

  // XXX: side-effecting random. It'll be nice to use a pure one!
  val factory: IStream[Candy, Random] = { rnd =>
    IStreamF(intToCandy(rnd.nextInt), rnd)
  }

  val board: Entity[BoardIn, BoardOut, (Board, Candy), (Board, Random)] =
    ((size |*| candies)
      .carrier[Board]
      .observe[Board]
      |*| factory)
        .in[BoardIn]
        .out[BoardOut]
        .inside(observeForReaction)
        .back(routeBackBoard)

  def score(limit: Nat): Entity[CounterIn, CounterOut, Nat, Nat] = { x =>
    EntityF(x, _ match {
      case Increase(n) if x + n > limit => (List(Done), None)
      case Increase(n) => (List.empty, Option(x + n))
      case Decrease(n) => (List.empty, Option(x - n))
    })
  }

  def level(
      target: Nat): Entity[BoardIn, CounterOut, Game, (Board, Random, Nat)] =
    (board.out[CounterIn] |->| score(target))
      .carrier[(Board, Random, Nat)]
      .observe(To { case ((b, r), n) => (b, n) })
}
