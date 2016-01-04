package org.hablapps.coalgebraz.candy

import org.hablapps.coalgebraz._

import Coalgebra._, CoentityOps._

object Cocandy {

  val cokey: Coentity[Unit, Nothing, String, String] = constant[String]

  val coflavour: Coistore[FlavourIn, Flavour, Flavour] =
    s => IStore(s, _ match {
      case Become(flavour) => flavour
    })

  val coposition: Coistore[PositionIn, (Int, Int), (Int, Int)] = {
    case s@(x, y) => IStore(s, _ match {
      case OverX(f) => (f(x), y)
      case OverY(f) => (x, f(y))
    })
  }

  val cocandy: Coentity[CandyIn, CandyOut, Candy, Candy] = ???
}
