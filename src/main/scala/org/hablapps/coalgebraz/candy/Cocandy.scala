package org.hablapps.coalgebraz.candy

import org.hablapps.coalgebraz._

object Cocandy {

  val coflavour: Coistore[FlavourIn, Flavour, Flavour] =
    s => IStore(s, fi => fi match { case Become(flavour) => flavour })

  val coposition: Coistore[PositionIn, (Int, Int), (Int, Int)] = {
    case s@(x, y) => IStore(s, pi => pi match {
      case OverX(f) => (f(x), y)
      case OverY(f) => (x, f(y))
    })
  }
}
