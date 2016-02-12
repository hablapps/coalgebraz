package org.hablapps.candy

import scala.util.Random

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

object Isos {

  implicit val toCandy = new (Random -> Candy) {
    val to: Random => Candy = rnd => intToCandy(rnd.nextInt)
  }

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
      case (size, candies) => Board(size, candies.map(c => (c.key, c)).toMap)
    }
    val from: Board => (Int, List[Candy]) = {
      case Board(size, candies) => (size, candies.values.toList)
    }
  }

  implicit val isoBoard2 = new ((Int, Map[String, Candy]) <-> Board) {
    val to: ((Int, Map[String, Candy])) => Board = {
      case (size, candies) => Board(size, candies)
    }
    val from: Board => (Int, Map[String, Candy]) = {
      case Board(size, candies) => (size, candies)
    }
  }
}
