package org.hablapps.coalgebraz.candy

sealed trait Flavour
case object Lemon extends Flavour { override def toString = "♠" }
case object Orange extends Flavour { override def toString = "♣" }
case object Mint extends Flavour { override def toString = "♥" }
case object Strawberry extends Flavour { override def toString = "♦" }
case object Banana extends Flavour { override def toString = "♤" }
case object Apple extends Flavour { override def toString = "♡" }
case object Melon extends Flavour { override def toString = "♢" }
case object Pear extends Flavour { override def toString = "♧" }

sealed trait FlavourIn
case class Become(flavour: Flavour) extends FlavourIn
