package org.hablapps

import scala.math.abs

import scalaz._, Scalaz._

package object candy {

  type CandyIn = CandyIn1 \/ CandyIn2

  type Game = (Board, Nat)

  // XXX: consider using `ScalaCheck` gens to avoid this crap
  def intToCandy(i: Int) = {
    val fl = abs(i) % 8 match {
      case 0 => Lemon
      case 1 => Orange
      case 2 => Mint
      case 3 => Coconut
      case 4 => Banana
      case 5 => Apple
      case 6 => Melon
      case 7 => Pear
    }
    Candy(i.toString, fl, (1, 1))
  }
}
