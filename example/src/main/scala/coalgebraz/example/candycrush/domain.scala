package coalgebraz.example.candycrush

import scala.util.Random

import scalaz._, Scalaz._

sealed trait Flavour
case object Lemon extends Flavour { override def toString = "♠" }
case object Orange extends Flavour { override def toString = "♣" }
case object Mint extends Flavour { override def toString = "♥" }
case object Coconut extends Flavour { override def toString = "♦" }
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

  val opposite = this match {
    case North => South
    case West  => East
    case South => North
    case East  => West
  }

  def apply(pos: (Int, Int)) = this match {
    case North => pos.map(_ - 1)
    case West  => pos.swap.map(_ - 1).swap
    case South => pos.map(_ + 1)
    case East  => pos.swap.map(_ + 1).swap
  }

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

case class Candy(key: String, flavour: Flavour, position: (Int, Int)) {
  override def toString = flavour.toString
}

sealed trait CandyIn
case class Fall(n: Int) extends CandyIn
case class Slide(direction: Direction) extends CandyIn
case class Mutate(flavour: Flavour) extends CandyIn
case object Crush extends CandyIn

sealed trait CandyOut
case object ByeCandy extends CandyOut

sealed trait CounterIn
case class Increase(n: Nat) extends CounterIn
case class Decrease(n: Nat) extends CounterIn

sealed trait CounterOut
case object Done extends CounterOut

case class Board(size: Int, candies: Map[String, Candy] = Map.empty)

sealed trait BoardIn
case class Transform(key: String, flavour: Flavour) extends BoardIn
case class Interchange(pos: (Int, Int), dir: Direction) extends BoardIn
case class NewCandy(candy: Candy) extends BoardIn
case class CrushThem(keys: NonEmptyList[String]) extends BoardIn

sealed trait BoardOut
case class Popped(n: Nat) extends BoardOut
case class Aligned(keys: NonEmptyList[String]) extends BoardOut
case class Suspended(pos: (Int, Int)) extends BoardOut
case class Inhabitated(pos: (Int, Int)) extends BoardOut
