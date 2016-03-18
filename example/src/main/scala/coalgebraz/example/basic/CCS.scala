package coalgebraz
package example.basic

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import shapeless._, shapeless.{ Coproduct, :+: }
import ops.coproduct._

import Coalgebraz._

object CCS extends App {

  // // Z =def= 0
  // def Z: CCS[Channel, Unit] = empty
  //
  // // runCCSIO(Z)(
  // //   (),
  // //   l => println(s"⇒ $l".toLowerCase),
  // //   r => println(s"⇒ _${r}_".toLowerCase))
  //
  // // CZ =def= coffee.Z
  // def CZ: CCS[Channel, Unit \/ Unit] = coffee.in %: Z
  //
  // // CTZ =def= coffee.tea.Z
  // def CTZ: CCS[Channel, (Unit \/ Unit) \/ (Unit \/ Unit)] =
  //   coffee.in %: tea.in %: Z

  def Z = empty3[Channel]

  def CZ = coffee.in %: Z

  def CTZ = coffee.in %: tea.in %: Z

  def CCZ = coffee.in %: coffee.in %: Z

  // No way!
  // def CS = pub.out %: coin.out %: coffee.in %: CS

  runCCSIO(CTZ)(
    // Inr(Inr(Inl(()))),
    Coproduct(()),
    l => println(s"⇒ $l".toLowerCase),
    r => println(s"⇒ _${r}_".toLowerCase))

  // runCCSIO(CoTZ)(
  //   (Coproduct(coffee.in, ()), Coproduct(tea.in, ())),
  //   l => println(s"⇒ $l".toLowerCase),
  //   r => println(s"⇒ _${r}_".toLowerCase))

  // CS =def= 'pub.'coin.coffee.Z
  // def CS = pub.out %: coin.out %: coffee.in %: CS

  // runCCSIO(CTZ)(
  //   // XXX: weird behaviour with `-\/(\/-(()))` which means that `coffee` needs
  //   // to be processed, but `tea` is done, so `coffee` lead us directly to `0`.
  //   // Could we develop a finer combinator to avoid rightmost elements to be \/-
  //   // when there's a previous -\/?
  //   -\/(-\/(())),
  //   l => println(s"⇒ $l".toLowerCase),
  //   r => println(s"⇒ _${r}_".toLowerCase))

  // // CTZ =def= coffee.tea.Z
  // def CTZ: CCS[Channel, (Unit \/ Unit) :: (Unit \/ Unit) :: HNil] =
  //   coffee.in %: tea.in %: Z
  //
  // // CS =def= 'pub.'coin.coffee.Z
  // def CS[X <: HList]: CCS[Channel, X] =
  //   pub.out %: coin.out %: coffee.in %: CS

  // runCCSIO(CTZ)(
  //   coffee.left[Unit] :: tea.left[Unit] :: HNil,
  //   l => println(s"⇒ $l".toLowerCase),
  //   r => println(s"⇒ _${r}_".toLowerCase))

  // // 0
  // empty
  //
  // // CF =def= coffee.tea.0
  // def CF: CCS[Channel, CFState] =
  //   (coffee.in -> CF1) %: (tea.in -> CF2) %: empty[Channel, CFState]
  //
  // // runCCSIO(CF)(
  // //   CF0,
  // //   l => println(s"⇒ $l".toLowerCase),
  // //   r => println(s"⇒ _${r}_".toLowerCase))
  //
  // // CS =def= _pub_._coin_.coffee.CS
  // def CS: CCS[Channel, CSState] =
  //   (pub.out -> CS1) %: (coin.out -> CS2) %: (coffee.in -> CS0) %: CS
  //
  // // runCCSIO(CS)(
  // //   CS0,
  // //   l => println(s"⇒ $l".toLowerCase),
  // //   r => println(s"⇒ _${r}_".toLowerCase))
  //
  // // CM =def= coin._coffee_.CM
  // def CM: CCS[Channel, CMState] =
  //   (coin.in -> CM1) %: (coffee.out -> CM0) %: CM
  //
  // // runCCSIO(CM)(
  // //   CM0,
  // //   l => println(s"⇒ $l".toLowerCase),
  // //   r => println(s"⇒ _${r}_".toLowerCase))
  //
  // // CTM =def= coin.(_coffee_.CTM + _tea_.CTM)
  // def CTM: CCS[Channel, CTMState] = (coin.in -> CTM1) %:
  //   ((coffee.out -> CTM0) %: CTM + (coffee.out -> CTM0) %: CTM)
  //
  // runCCSIO(CTM)(
  //   CTM0,
  //   l => println(s"⇒ $l".toLowerCase),
  //   r => println(s"⇒ _${r}_".toLowerCase))
  //
  // // RCTM =def= CTM \ tea
  // def RCTM: CCS[Channel, CTMState] = CTM \ tea
  //
  // // CS | CM
  // def cs_cm: CCS[Channel \/ Channel, (CSState, CMState)] = CS | CM
  //
  // // CS | CS'
  // def cs_cs: CCS[Channel \/ Channel, (CSState, CSState)] = CS | CS
  //
  // // TODO: CS | CS' | CM
  //
  // // TODO: CS | CM | CM'
  //
  // // SmUni =def= (CS | CM) \ coin \ coffee
  // def SmUni: CCS[Channel \/ Channel, (CSState, CMState)] =
  //   (CS | CM) \ coin.right \ coffee.right
  //
  // // runCCSIO(SmUni)(
  // //   (CS0, CM0),
  // //   l1 => println(s"⇒ ${ l1.fold(
  // //     l2 => s"<| $l2",
  // //     r2 => s"|> $r2") }".toLowerCase),
  // //   r1 => println(s"⇒ ${ r1.fold(
  // //     l2 => s"<| _${l2}_",
  // //     r2 => s"|> _${r2}_") }".toLowerCase))
  //
  // // VM  =def= coin._item_.VM
  // def VM: CCS[GenChannel, GenState] =
  //   (item0.in -> S1) %: (item1.out -> S0) %: VM
  //
  // // CM_2 =def= VM[coffee/item]
  // def CM_2: CCS[Channel, CMState] =
  //   VM r (coin / item0, coffee / item1)
  //
  // // runCCSIO(CM_2)(
  // //   CM0,
  // //   l => println(s"⇒ $l".toLowerCase),
  // //   r => println(s"⇒ _${r}_".toLowerCase))
  //
  // sealed trait CFState
  // case object CF0 extends CFState
  // case object CF1 extends CFState
  // case object CF2 extends CFState
  //
  // object CFState {
  //   implicit val orderedInstance: Ordered[CFState] = new Ordered[CFState] {
  //     def min = CF0
  //     def max = CF2
  //     def compare(a1: CFState, a2: CFState) = a1.toString compare a2.toString
  //   }
  // }
  //
  // sealed trait CSState
  // case object CS0 extends CSState
  // case object CS1 extends CSState
  // case object CS2 extends CSState
  //
  // object CSState {
  //   implicit val orderedInstance: Ordered[CSState] = new Ordered[CSState] {
  //     def min = CS0
  //     def max = CS2
  //     def compare(a1: CSState, a2: CSState) = a1.toString compare a2.toString
  //   }
  // }
  //
  // sealed trait CMState
  // case object CM0 extends CMState
  // case object CM1 extends CMState
  //
  // object CMState {
  //
  //   implicit val orderedInstance: Ordered[CMState] = new Ordered[CMState] {
  //     def min = CM0
  //     def max = CM1
  //     def compare(a1: CMState, a2: CMState) = a1.toString compare a2.toString
  //   }
  //
  //   implicit val isoGenCMState: Iso[GenState, CMState] =
  //     Iso[GenState, CMState](
  //       { case S0 => CM0
  //         case S1 => CM1 },
  //       { case CM0 => S0
  //         case CM1 => S1 })
  // }
  //
  // sealed trait CTMState
  // case object CTM0 extends CTMState
  // case object CTM1 extends CTMState
  //
  // object CTMState {
  //   implicit val orderedInstance: Ordered[CTMState] = new Ordered[CTMState] {
  //     def min = CTM0
  //     def max = CTM1
  //     def compare(a1: CTMState, a2: CTMState) = a1.toString compare a2.toString
  //   }
  // }
  //
  // sealed trait GenState
  // case object S0 extends GenState
  // case object S1 extends GenState
  // // ...
  // // case object SN extends GenState
  //
  // object GenState {
  //   implicit val orderedInstance: Ordered[GenState] = new Ordered[GenState] {
  //     def min = S0
  //     def max = S1
  //     // XXX: haha, what a crap!
  //     def compare(a1: GenState, a2: GenState) = a1.toString compare a2.toString
  //   }
  // }
  //
  // sealed trait GenChannel
  // case object item0 extends GenChannel
  // case object item1 extends GenChannel
  // // ...
  // // case object itemN extends GenChannel

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
          case left(act)  =>
            readChannel.read(act).map(_.bimap(_.left, _.left))
          case right(act) =>
            readChannel.read(act).map(_.bimap(_.right, _.right))
          case _ => Option.empty
        }
      }
  }
}