package coalgebraz
package example.basic

import scalaz._, Scalaz._

import Coalgebraz._

object Milner extends App {

  val cs: Milner[Channel, CSState] = milner {
    case CS0 => List(pub.right -> CS1)
    case CS1 => List(coin.right -> CS2)
    case CS2 => List(coffee.left -> CS0)
  }

  val cm: Milner[Channel, CMState] = milner {
    case CM0 => List(coin.left -> CM1)
    case CM1 => List(coffee.right -> CM0)
  }

  trait CSState
  case object CS0 extends CSState
  case object CS1 extends CSState
  case object CS2 extends CSState

  trait CMState
  case object CM0 extends CMState
  case object CM1 extends CMState

  trait Channel
  case object pub extends Channel
  case object coin extends Channel
  case object coffee extends Channel
}
