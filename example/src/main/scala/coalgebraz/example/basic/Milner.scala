package coalgebraz
package example.basic

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import Coalgebraz._

// The following examples were obtained from these nice lecture notes:
// http://people.cis.ksu.edu/~schmidt/705a/Lectures/intro2ccs.pdf
object Milner extends App {

  // 0
  empty

  // CF =def= coffee._tea_.0
  def CF: Milner[Channel, CFState] =
    (coffee.in -> CF1) %:
      (tea.out -> CF2) %:
      (tea.in -> CF3 /* should I care? */) %:
      empty[Channel, CFState]

  runMilnerIO(CF)(CF0, l => println(s"⇒ $l"), r => println(s"⇒ _${r}_"))

  // CS =def= _pub_._coin_.coffee.CS

  def CS: Milner[Channel, CSState] = milner {
    case CS0 => pub.out % CS1
    case CS1 => coin.out % CS2
    case CS2 => coffee.in % CS0
  }

  def CS_2: Milner[Channel, CSState] =
    (pub.out -> CS1) %: (coin.out -> CS2) %: (coffee.in -> CS0) %: CS_2

  // runMilnerIO(CS_2)(CS0, l => println(s"⇒ $l"), r => println(s"⇒ _${r}_"))

  // CM =def= coin._coffee_.CM
  def CM: Milner[Channel, CMState] = milner {
    case CM0 => coin.in % CM1
    case CM1 => coffee.out % CM0
  }

  // CTM =def= coin.(_coffee_.CTM + _tea_.CTM)
  def CTM: Milner[Channel, CTMState] = milner {
    case CTM0 => coin.in % CTM1
    case CTM1 => (coffee.out % CTM0) ++ (tea.out % CTM0)
  }

  // RCTM =def= CTM \ tea
  def RCTM: Milner[Channel, CTMState] = CTM \ tea

  // CS | CM
  def cs_cm: Milner[Channel \/ Channel, (CSState, CMState)] = CS | CM

  // CS | CS'
  def cs_cs: Milner[Channel \/ Channel, (CSState, CSState)] = CS | CS

  // TODO: CS | CS' | CM

  // TODO: CS | CM | CM'

  // SmUni =def= (CS | CM) \ coin \ coffee
  def SmUni: Milner[Channel \/ Channel, (CSState, CMState)] =
    (CS | CM) \ coin.right \ coffee.right

  // VM  =def= coin._item_.VM
  def VM: Milner[GenChannel, GenState] = milner {
    case S0 => item0.in  % S1
    case S1 => item1.out % S0
  }

  // CM2 =def= VM[coffee/item]
  def CM2: Milner[Channel, CMState] =
    VM rename (coin / item0, coffee / item1)

  // runMilnerIO(SmUni)(
  //   (CS0, CM0),
  //   l => println(s"⇒ <| ${ l.fold(_.toString, "_" + _ + "_") }"),
  //   r => println(s"⇒ |> ${ r.fold(_.toString, "_" + _ + "_") }"))

  // runMilnerIO(CM2)(CM0, l => println(s"⇒ $l"), r => println(s"⇒ _${r}_"))

  sealed trait CFState
  case object CF0 extends CFState
  case object CF1 extends CFState
  case object CF2 extends CFState
  case object CF3 extends CFState

  object CFState {
    implicit val orderedInstance: Ordered[CFState] = new Ordered[CFState] {
      // XXX: haha, what a crap!
      def compare(a1: CFState, a2: CFState) = a1.toString compare a2.toString
    }
  }

  sealed trait CSState
  case object CS0 extends CSState
  case object CS1 extends CSState
  case object CS2 extends CSState

  object CSState {
    implicit val orderedInstance: Ordered[CSState] = new Ordered[CSState] {
      def compare(a1: CSState, a2: CSState) = a1.toString compare a2.toString
    }
  }

  sealed trait CMState
  case object CM0 extends CMState
  case object CM1 extends CMState

  object CMState {
    implicit val isoGenCMState: Iso[GenState, CMState] =
      Iso[GenState, CMState](
        { case S0 => CM0
          case S1 => CM1 },
        { case CM0 => S0
          case CM1 => S1 })
  }

  sealed trait CTMState
  case object CTM0 extends CTMState
  case object CTM1 extends CTMState

  sealed trait GenState
  case object S0 extends GenState
  case object S1 extends GenState
  // ...
  // case object SN extends GenState

  sealed trait GenChannel
  case object item0 extends GenChannel
  case object item1 extends GenChannel
  // ...
  // case object itemN extends GenChannel

  sealed trait Channel
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
