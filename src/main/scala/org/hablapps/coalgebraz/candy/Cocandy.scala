package org.hablapps.coalgebraz.candy

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._

import Coalgebra._, CoentityOps._

object Cocandy {

  val cokey: Coentity[Void, Void, String, String] = constant[String]

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

  val cotest: Coentity[
      FlavourIn \/ PositionIn,
      Void,
      ((String, Flavour), (Int, Int)),
      ((String, Flavour), (Int, Int))] =
    cokey |+| coflavour |+| coposition
}
