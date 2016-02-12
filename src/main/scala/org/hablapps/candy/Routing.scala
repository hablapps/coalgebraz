package org.hablapps.candy

import scala.language.implicitConversions

import scala.collection.immutable.Stream
import scala.util.Random

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Routing {

  implicit def routeInCandy(
      candy: Candy)(
      in: CandyIn): List[FlavourIn \/ PositionIn] = in match {
    case Fall(n)    => List(OverY(_ + n).right)
    case Slide(dir) => List(dir.toPositionIn.right)
    case Mutate(fl) => List(Become(fl).left)
    case _          => List.empty
  }

  implicit def routeOutCandy(candy: Candy)(in: Void): List[CandyOut] =
    List.empty

  implicit def routeInBoard(
      obs: (Board, Candy))(
      in: BoardIn): List[IndexIn[CandyIn, Candy, String] \/ Unit] = in match {
    case Transform(k, fl) => List(-\/(WrapIn((k, Mutate(fl)))))
    case Interchange(pos, dir) => {
      val candies = obs._1.candies.values
      def f(p: (Int, Int)): Option[String] =
        candies.find(_.position == p).map(_.key)
      (f(pos) |@| f(dir(pos))) { (k1, k2) =>
        List[IndexIn[CandyIn, Candy, String] \/ Unit](
          -\/(WrapIn((k1, Slide(dir)))),
          -\/(WrapIn((k2, Slide(dir.opposite)))))
      }.getOrElse(List.empty)
    }
    case NewCandy(candy) => List(-\/(Attach(candy)), \/-(()))
    case CrushThem(keys) => keys.toList.map { k =>
      -\/(WrapIn((k, Crush))): IndexIn[CandyIn, Candy, String] \/ Unit
    }
  }

  implicit def routeOutBoard(
      obs: (Board, Candy))(
      out: IndexOut[CandyOut, Candy, String]): List[BoardOut] = out match {
    case WrapOut((_, ByeCandy)) => List(Popped(1))
    case _ => List.empty
  }

  implicit def routeBackBoard(
      obs: (Board, Candy))(
      out: BoardOut): List[BoardIn] = out match {
    case Aligned(keys)    => List(CrushThem(keys))
    case Suspended(pos)   => List(Interchange(pos, South))
    case Inhabitated(pos) => List(NewCandy(obs._2.copy(position = pos)))
    case _ => List.empty
  }

  implicit def routeInScore(
      obs: Nat)(
      in: BoardOut): List[CounterIn] = in match {
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
