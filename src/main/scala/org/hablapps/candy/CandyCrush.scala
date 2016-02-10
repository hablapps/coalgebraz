package org.hablapps.candy

import scala.util.Random
import Function.const

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import org.hablapps.coalgebraz.StoreF
import Coalgebraz._, EntityOps._
import Sq.someOrNone
import Nat.Syntax._
import Isos.{ isoCandy, isoBoard }, To.eqTo
import Adapt._
import Routing._

object CandyCrush {

  val key: Entity[Void, Void, String, String] = blocked(eqTo)

  val flavour: IStore[FlavourIn, Flavour, Flavour] = { x =>
    IStoreF(x, _ match {
      case Become(flavour) => flavour
    })
  }

  val position: IStore[PositionIn, (Int, Int), (Int, Int)] = {
    case s@(x, y) => IStoreF(s, _ match {
      case OverX(f) => (f(x), y)
      case OverY(f) => (x, f(y))
    })
  }

  val icandy: Entity[CandyIn, CandyOut, Candy, Candy] =
    key |*| flavour |*| position

  val candy: Entity[CandyIn, CandyOut, Candy, Candy] =
    icandy |~| (_ == Crush, _ => _ => List(ByeCandy))

  val candies: EntitySeq[CandyIn, CandyOut, Candy, Candy] =
    candy.toCoseq

  val size: Entity[Void, Void, Int, Int] = blocked(eqTo)

  // XXX: side-effecting random. It'll be nice to use a pure one!
  val factory: IStream[Candy, Random] = { rnd =>
    IStreamF(intToCandy(rnd.nextInt), rnd)
  }

  val stableBoard: Entity[BoardIn, BoardOut, (Board, Candy), (Board, Random)] =
    size |*| candies |*| factory

  val board: Entity[BoardIn, BoardOut, (Board, Candy), (Board, Random)] =
    stableBoard
      .inside(observeForReaction)
      .back(routeBackBoard)

  def score(limit: Nat): Entity[CounterIn, CounterOut, Nat, Nat] =
    raw(identity, x => _ match {
      case Increase(n) if x + n > limit => (List(Done), None)
      case Increase(n) => (List.empty, Option(x + n))
      case Decrease(n) => (List.empty, Option(x - n))
    })

  def level(
      target: Nat): Entity[BoardIn, CounterOut, Game, (Board, Random, Nat)] =
    board |>| score(target)
}
