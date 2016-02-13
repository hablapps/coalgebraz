package org.hablapps.candy

import scala.language.implicitConversions

import scala.collection.immutable.Stream
import scala.util.Random

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Routing {

  implicit val routeInCandy: Router[Candy, CandyIn, FlavourIn \/ PositionIn] =
    candy => {
      case Fall(n)    => List(OverY(_ + n).right)
      case Slide(dir) => List(dir.toPositionIn.right)
      case Mutate(fl) => List(Become(fl).left)
      case _          => List.empty
    }

  implicit val routeOutCandy: Router[Candy, Void, CandyOut] =
    _ => _ => List.empty

  implicit val routeInBoard: Router[
      (Board, Candy),
      BoardIn,
      IndexIn[CandyIn, Candy, String] \/ Unit] = obs => {
    case Transform(k, fl) =>
      List(WrapIn[CandyIn, Candy, String]((k, Mutate(fl))).left)
    case Interchange(pos, dir) => {
      val candies = obs._1.candies.values
      def f(
          p: (Int, Int),
          d: Direction): Option[IndexIn[CandyIn, Candy, String] \/ Unit] =
        candies.find(_.position == p).map {
          c => WrapIn[CandyIn, Candy, String]((c.key, Slide(d))).left
        }
      f(pos, dir).toList ++ f(dir(pos), dir.opposite).toList
    }
    case NewCandy(candy) => List(Attach(candy).left, ().right)
    case CrushThem(keys) => keys.toList.map { k =>
      WrapIn[CandyIn, Candy, String]((k, Crush)).left
    }
  }

  implicit val routeOutBoard: Router[
      (Board, Candy),
      IndexOut[CandyOut, Candy, String],
      BoardOut] = obs => {
    case WrapOut((_, ByeCandy)) => List(Popped(1))
    case _ => List.empty
  }

  implicit val routeBackBoard: Router[
      (Board, Candy),
      BoardOut,
      BoardIn] = obs => {
    case Aligned(keys)    => List(CrushThem(keys))
    case Suspended(pos)   => List(Interchange(pos, South))
    case Inhabitated(pos) => List(NewCandy(obs._2.copy(position = pos)))
    case _ => List.empty
  }

  implicit val routeInScore: Router[Nat, BoardOut, CounterIn] = obs => {
    case Popped(n) => List(Increase(n))
    case _ => List.empty
  }

  def observeForReaction(beh: (Board, Candy)): List[BoardOut] =
    Stream(observeForGravitate _, observeForPopulate _, observeForCrush _)
      .map(_(beh._1))
      .find(_.isDefined)
      .flatten
      .toList

  private def observeForGravitate(board: Board): Option[Suspended] =
    board.candies.values.find { c1 =>
      c1.position._2 != board.size && (! board.candies.values.exists { c2 =>
        c2.position == c1.position.map(_ + 1)
      })
    }.map(c => Suspended(c.position))

  private def observeForPopulate(board: Board): Option[Inhabitated] = {
    val Board(size, candies) = board
    (1 to size).toStream
      .map((_, 1))
      .find(pos => ! candies.values.exists(_.position == pos))
      .map(Inhabitated(_))
  }

  // XXX: assumes a full board
  private def observeForCrush(board: Board): Option[Aligned] = {

    def align(f: Candy => Int, g: Candy => Int): List[Candy] =
      board.candies.values.toList
        .groupBy(f)
        .mapValues { vs =>
          vs.sortWith((c1, c2) => g(c1) < g(c2))
            .groupWhen((c1, c2) => c1.flavour == c2.flavour)
            .map(_.toList)
            .filter(_.size >= 3)
            .flatten
        }
        .values
        .toList
        .flatten

    (align(_.position._1, _.position._2) ++ align(_.position._2, _.position._1))
      .distinct
      .foldLeft(Option.empty[Aligned]) {
        case (Some(Aligned(nel)), c) =>
          Some(Aligned(nel.append(NonEmptyList(c.key))))
        case (None, c) => Some(Aligned(NonEmptyList(c.key)))
      }
  }
}
