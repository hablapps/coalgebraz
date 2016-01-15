package org.hablapps.coalgebraz.candy

import scala.util.Random

import org.hablapps.coalgebraz._
import Driver.runIO
import Cocandy._

object IO extends App {

  def printGame(game: Game): Unit = ???

  implicit val readBoardIn: Read[BoardIn] = ???

  runIO(cogame(10), (Board(???, ???), new Random(), Nat(0)), printGame)
}
