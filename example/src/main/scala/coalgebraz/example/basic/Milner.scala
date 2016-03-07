package coalgebraz
package example.basic

import scalaz._, Scalaz._

import Coalgebraz._

object Milner extends App {

  val cs: Milner[CSChannel, CSState] = milner {
    case CS0 => List(_pub_  -> CS1)
    case CS1 => List(_coin_ -> CS2)
    case CS2 => List(coffee -> CS0)
  }

  trait CSState
  case object CS0 extends CSState
  case object CS1 extends CSState
  case object CS2 extends CSState

  trait CSChannel
  case object _pub_ extends CSChannel
  case object _coin_ extends CSChannel
  case object coffee extends CSChannel
}
