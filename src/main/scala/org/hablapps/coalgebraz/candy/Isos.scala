package org.hablapps.coalgebraz.candy

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Isos {

  implicit val isoCandy = new ((String, Flavour, (Int, Int)) <-> Candy) {
    val to: ((String, Flavour, (Int, Int))) => Candy = {
      case (key, fla, pos) => Candy(key, fla, pos)
    }
    val from: Candy => (String, Flavour, (Int, Int)) = {
      case Candy(key, fla, pos) => (key, fla, pos)
    }
  }

  implicit val isoBoard = new ((Int, List[Candy]) <-> Board) {
    val to: ((Int, List[Candy])) => Board = {
      case (size, candies) => Board(size, candies)
    }
    val from: Board => (Int, List[Candy]) = {
      case Board(size, candies) => (size, candies)
    }
  }
}
