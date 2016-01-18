package org.hablapps.coalgebraz.candy

import scala.language.implicitConversions

import scala.collection.immutable.Stream
import scala.util.Random

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Routing {

  implicit def routeInCandy1(
      candy: Candy)(
      in: CandyIn1): List[FlavourIn \/ PositionIn] = in match {
    case Fall(n)    => List(OverY(_ + n).right)
    case Slide(dir) => List(dir.toPositionIn.right)
    case Mutate(fl) => List(Become(fl).left)
  }

  implicit def routeInBoard(
      obs: (Board, Candy))(
      in: BoardIn): List[CoseqIn[CandyIn, Candy, Candy] \/ Unit] = in match {
    case Transform(key, flavour) => List(-\/(Elem {
      case Candy(`key`, _, _) => Option(Mutate(flavour).left)
      case _ => None
    }))
    case Interchange(pos, dir) => ???
    case NewCandy(candy) => List(-\/(Prepend(candy)), \/-(()))
    case CrushThem(keys) => List(-\/(Elem {
      case Candy(k, _, _) if keys contains k => Option(Crush.right)
      case _ => None
    }))
  }

  implicit def routeOutBoard(
      obs: (Board, Candy))(
      out: CoseqOut[CandyOut, Candy]): List[BoardOut] = {
    val bos: List[BoardOut] = observeForReaction(obs._1).toList
    bos ++ (out match {
      case WrappedOut(os) => {
        val n = os.list.foldLeft(0)((acc, ByeCandy) => acc + 1)
        if (n > 0) List(Popped(n)) else List.empty
      }
      case _ => List.empty
    })
  }

  implicit def routeBackBoard(
      obs: (Board, Candy))(
      out: BoardOut): List[BoardIn] = out match {
    case Aligned(keys)    => List(CrushThem(keys))
    case Suspended(pos)   => List(Interchange(pos, South))
    case Inhabitated(pos) => List(NewCandy(obs._2.copy(position = pos)))
    case _ => List.empty
  }

  implicit def routeOutBoard2(
      obs: (Board, Candy))(
      out: BoardOut): List[CounterIn] = out match {
    case Popped(n) => List(Increase(n))
    case _ => List.empty
  }

  /* Reactions */

  private def observeForReaction(board: Board): Option[BoardOut] =
    Stream(observeForGravitate _, observeForPopulate _, observeForCrush _)
      .map(_(board))
      .find(_.isDefined)
      .flatten

  private def observeForGravitate(board: Board): Option[Suspended] =
    board.candies.find { c1 =>
      c1.position._2 != board.size && board.candies.exists { c2 =>
        c2.position == c1.position.map(_ + 1)
      }
    }.map(c => Suspended(c.position))

  private def observeForPopulate(board: Board): Option[Inhabitated] = {
    val Board(size, candies) = board
    (1 to size).toStream
      .map((_, 1))
      .find(pos => candies.exists(_.position == pos))
      .map(Inhabitated(_))
  }

  private def observeForCrush(board: Board): Option[Aligned] = ???
}
