package org.hablapps.candy

import scala.util.Random

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Driver.runIO
import Cocandy._

object IO extends App {

  def printGame(game: Game): Unit = {
    print("  ")
    println((1 to game._1.size).mkString(" "))
    (1 to game._1.size) foreach { y =>
      print(s"$y ")
      print(((1 to game._1.size) map { x =>
        game._1.candies.find(_.position == (x, y)).fold("-")(_.toString)
      }).mkString(" "))
      println()
    }
    println(s"Points: ${game._2}\n")
  }

  implicit val readDirection: Read[Direction] = new Read[Direction] {
    def read(s: String) = s match {
      case "up"    => Option(North)
      case "down"  => Option(South)
      case "left"  => Option(West)
      case "right" => Option(East)
      case _ => None
    }
  }

  // XXX: far from safe, but good enough to toy around
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

  /* Crush them all! */

  println("""
    |   _____                _          _____                _
    |  / ____|              | |        / ____|              | |
    | | |     __ _ _ __   __| |_   _  | |     _ __ _   _ ___| |__
    | | |    / _` | '_ \ / _` | | | | | |    | '__| | | / __| '_ \
    | | |___| (_| | | | | (_| | |_| | | |____| |  | |_| \__ \ | | |
    |  \_____\__,_|_| |_|\__,_|\__, |  \_____|_|   \__,_|___/_| |_|
    |                           __/ |
    |                          |___/
    |""".stripMargin)

  val target = 15

  println(s"### TARGET: $target points ###\n")

  runIO(
    game(target),
    (Board(8, List(Candy("one", Lemon, (1, 1)))), new Random(), Nat(0)),
    printGame)
}
