package coalgebraz.example.candycrush

import scala.util.Random
import Function.const

import scalaz._, Scalaz._

import coalgebraz._
import coalgebraz.StoreF
import Coalgebraz._, dsl._
import Nat.Syntax._
import Isos.{ toCandy, isoCandy, isoBoard, isoBoard2 }, To.eqTo
import Adapt._
import Routing._

object CandyCrush {

  val key: Entity[Void, Void, String, String] = blocked(eqTo)

  val flavour: Entity[FlavourIn, Void, Flavour, Flavour] =
    next(implicit x => {
      case Become(flavour) => flavour
    })

  val position: Entity[PositionIn, Void, (Int, Int), (Int, Int)] =
    next(implicit xy => {
      case OverX(f) => xy.swap.map(f).swap
      case OverY(f) => xy map f
    })

  val icandy: Entity[CandyIn, CandyOut, Candy, Candy] =
    key |*| flavour |*| position

  val candy: Entity[CandyIn, CandyOut, Candy, Candy] =
    icandy |~| (_ == Crush, _ => _ => List(ByeCandy))

  val candies: IndexedEntity[CandyIn, CandyOut, Candy, Candy, String] =
    candy.index(_.key, identity)

  val size: Entity[Void, Void, Int, Int] = blocked(eqTo)

  // XXX: side-effecting random. It'll be nice to use a pure one!
  // XXX: ops, a little help with type inference is required!
  val factory: Entity[Unit, Void, Candy, Random] =
    next[Unit, Void, Random](implicit x => {
      case _ => skip
    })

  val stableBoard: Entity[BoardIn, BoardOut, (Board, Candy), (Board, Random)] =
    size |*| candies |*| factory

  val board: Entity[BoardIn, BoardOut, (Board, Candy), (Board, Random)] =
    stableBoard
      .inside(observeForReaction)
      .back(routeBackBoard)

  def score(limit: Nat): Entity[CounterIn, CounterOut, Nat, Nat] =
    next(implicit x => {
      case Increase(n) if x + n > limit => halt ~> Done
      case Increase(n) => x + n
      case Decrease(n) => x - n
    })

  def level(
      target: Nat): Entity[BoardIn, CounterOut, Game, (Board, Random, Nat)] =
    board |>| score(target)
}
