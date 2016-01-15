package org.hablapps.coalgebraz.candy

import scala.util.Random

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Driver.runIO
import Cocandy._

object IO extends App {

  def printGame(game: Game): Unit = ???

  implicit val readDirection: Read[Direction] = new Read[Direction] {
    def read(s: String) = s match {
      case "up"    => Option(North)
      case "down"  => Option(South)
      case "left"  => Option(West)
      case "right" => Option(East)
      case _ => None
    }
  }

  implicit val readBoardIn: Read[BoardIn] = new Read[BoardIn] {
    def read(s: String) = {
      s.split(" ") match {
        case Array(a, b, c) => {
          (Read[Int].read(a)
           |@| Read[Int].read(b)
           |@| Read[Direction].read(c)) { (x, y, dir) =>
             Interchange((x, y), dir)
           }
        }
        case _ => None
      }
    }
  }

  runIO(
    cogame(10),
    (Board(8, List.empty), new Random(), Nat(0)),
    printGame)
}
