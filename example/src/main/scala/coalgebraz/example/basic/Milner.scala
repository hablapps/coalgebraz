package coalgebraz
package example.basic

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import Coalgebraz._

// The following examples have been obtained from these lecture notes:
// http://people.cis.ksu.edu/~schmidt/705a/Lectures/intro2ccs.pdf
object Milner extends App {

  // CS =def= _pub_._coin_.coffee.CS
  def CS: Milner[Channel, CSState] = milner {
    case CS0 => LazyList(pub.out -> CS1)
    case CS1 => LazyList(coin.out -> CS2)
    case CS2 => LazyList(coffee.in -> CS0)
  }

  // CM =def= coin._coffee_.CM
  def CM: Milner[Channel, CMState] = milner {
    case CM0 => LazyList(coin.in -> CM1)
    case CM1 => LazyList(coffee.out -> CM0)
  }

  // CTM =def= coin.(_coffee_.CTM + _tea_.CTM)
  def CTM: Milner[Channel, CTMState] = milner {
    case CTM0 => LazyList(coin.in -> CTM1)
    case CTM1 => LazyList(coffee.out -> CTM0, tea.out -> CTM0)
  }

  // CS | CM
  def cs_cm: Milner[Channel \/ Channel, (CSState, CMState)] = CS | CM

  // CS | CS'
  def cs_cs: Milner[Channel \/ Channel, (CSState, CSState)] = CS | CS

  // TODO: CS | CS' | CM

  // TODO: CS | CM | CM'

  runMilnerIO(cs_cs)(
    (CS0, CS0),
    l => println(s"⇒ <| ${ l.fold(_.toString, "_" + _ + "_") }"),
    r => println(s"⇒ |> ${ r.fold(_.toString, "_" + _ + "_") }"))

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
          case "pub" => pub.in.some
          case "coin" => coin.in.some
          case "coffee" => coffee.in.some
          case "tea" => tea.in.some
          case "_pub_" => pub.out.some
          case "_coin_" => coin.out.some
          case "_coffee_" => coffee.out.some
          case "_tea_" => tea.out.some
          case _ => Option.empty
        }
      }

    implicit val readChannel2: Read[(Channel \/ Channel) \/ (Channel \/ Channel)] =
      new Read[(Channel \/ Channel) \/ (Channel \/ Channel)] {
        val left  = """<\| (.*)""".r
        val right = """\|> (.*)""".r
        def read(s: String): Option[(Channel \/ Channel) \/ (Channel \/ Channel)] = s match {
          case left(act)  => readChannel.read(act).map(_.left)
          case right(act) => readChannel.read(act).map(_.right)
          case _ => Option.empty
        }
      }
  }
}
