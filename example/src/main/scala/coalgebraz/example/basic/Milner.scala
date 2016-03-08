package coalgebraz
package example.basic

import scalaz._, Scalaz._

import Coalgebraz._

object Milner extends App {

  // def CS = _pub_._coin_.coffee.CS
  val cs: Milner[Channel, CSState] = milner {
    case CS0 => List(pub.right -> CS1)
    case CS1 => List(coin.right -> CS2)
    case CS2 => List(coffee.left -> CS0)
  }

  runMilnerIO(cs)(CS0)

  // def CM = coin._coffee_.CM
  val cm: Milner[Channel, CMState] = milner {
    case CM0 => List(coin.left -> CM1)
    case CM1 => List(coffee.right -> CM0)
  }

  // def CTM = coin.(_coffee_.CTM + _tea_.CTM)
  val ctm: Milner[Channel, CTMState] = milner {
    case CTM0 => List(coin.left -> CTM1)
    // non-determinism
    case CTM1 => List(coffee.right -> CTM0, tea.right -> CTM0)
  }

  // CS | CM
  val cs_cm: Milner[Channel \/ Channel, (CSState, CMState)] = cs | cm

  trait CSState
  case object CS0 extends CSState
  case object CS1 extends CSState
  case object CS2 extends CSState

  trait CMState
  case object CM0 extends CMState
  case object CM1 extends CMState

  trait CTMState
  case object CTM0 extends CTMState
  case object CTM1 extends CTMState

  trait Channel
  case object pub extends Channel
  case object coin extends Channel
  case object coffee extends Channel
  case object tea extends Channel

  object Channel {
    implicit val readChannel: Read[Channel \/ Channel] =
      new Read[Channel \/ Channel] {
        def read(s: String): Option[Channel \/ Channel] = s match {
          case "pub" => pub.left.some
          case "coin" => coin.left.some
          case "coffee" => coffee.left.some
          case "tea" => tea.left.some
          case "_pub_" => pub.right.some
          case "_coin_" => coin.right.some
          case "_coffee_" => coffee.right.some
          case "_tea_" => tea.right.some
          case _ => Option.empty
        }
      }
  }
}
