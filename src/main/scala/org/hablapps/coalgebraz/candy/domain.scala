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

sealed trait PositionIn
case class OverX(f: Int => Int) extends PositionIn
case class OverY(f: Int => Int) extends PositionIn

sealed trait Direction {
  def toPositionIn: PositionIn = this match {
    case North => OverY(_ - 1)
    case West  => OverX(_ - 1)
    case South => OverY(_ + 1)
    case East  => OverX(_ + 1)
  }
}
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

case class Candy(key: String, flavour: Flavour, position: (Int, Int))

sealed trait CandyIn1
case class Fall(n: Int) extends CandyIn1
case class Slide(direction: Direction) extends CandyIn1
case class Mutate(flavour: Flavour) extends CandyIn1

sealed trait CandyIn2
case object Crush extends CandyIn2

sealed trait CandyOut
case object ByeCandy extends CandyOut
