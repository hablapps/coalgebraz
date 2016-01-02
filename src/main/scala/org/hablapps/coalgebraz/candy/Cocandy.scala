package org.hablapps.coalgebraz.candy

import org.hablapps.coalgebraz._

object Cocandy {

  val coflavour: Coistore[FlavourIn, Flavour, Flavour] =
    s => IStore(s, fi => fi match { case Become(flavour) => flavour })
}
