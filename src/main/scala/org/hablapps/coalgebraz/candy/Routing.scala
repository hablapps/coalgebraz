package org.hablapps.coalgebraz.candy

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Routing {

  def routeInCandy1(
      candy: Candy)(
      in: CandyIn1): List[FlavourIn \/ PositionIn] = in match {
    case Fall(n)    => List(OverY(_ + n).right)
    case Slide(dir) => List(dir.toPositionIn.right)
    case Mutate(fl) => List(Become(fl).left)
  }

  def routeInBoard(
      board: Board)(
      in: BoardIn): List[CoseqIn[CandyIn, Candy, Candy] \/ Unit] = in match {
    case Transform(key, flavour) => List(-\/(Elem {
      case Candy(`key`, _, _) => Option(Mutate(flavour).left)
      case _ => None
    }))
    case Interchange(pos, dir) => ???
    case NewCandy(candy) => List(-\/(Prepend(candy)))
    case CrushThem(keys) => List(-\/(Elem {
      case Candy(k, _, _) if keys contains k => Option(Crush.right)
      case _ => None
    }))
  }

  def routeOutBoard(
      board: Board)(
      out: CoseqOut[CandyOut, Candy]): List[BoardOut] = out match {
    case WrappedOut(os) => {
      val n = os.list.foldLeft(0)((acc, ByeCandy) => acc + 1)
      if (n > 0) List(Popped(n)) else List.empty
    }
    case _ => List.empty
  }

  def routeBackBoard(board: Board)(out: BoardOut): Option[BoardIn] = ???
}
