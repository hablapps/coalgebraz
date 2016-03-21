package coalgebraz
package example.basic

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import shapeless._, shapeless.{ Coproduct, :+: }
import ops.coproduct._

import Coalgebraz._

object CCS extends App {

  def Z = empty[Channel, Unit]

  def CZ = coffee.in %: Z

  def CTZ = coffee.in %: tea.in %: Z

  // No way!
  // def CS = pub.out %: coin.out %: coffee.in %: CS

  def CoTZ = (coffee.out %: Z) + (tea.out %: Z)

  def CCoTZ = coin.in %: ((coffee.out %: Z) + (tea.out %: Z))

  def CCZ = CCoTZ \ Set(tea)

  def VM = coin.in %: item.out %: Z

  def CHM = VM / { case `item` => chocs }
  def DFM = VM / { case `item` => figs }
  def CRM = VM / { case `item` => crisps }

  def CS = pub.out %: coin.out %: coffee.in %: Z

  def CM = coin.in %: coffee.out %: Z

  def SmUni = (CS | CM) \ Set(coin, coffee)

  def SmUni2 = (CS | CS | CM) \ Set(coin, coffee)

  runCCSIO(SmUni2)(
    (Inl(Inl(Inl(()))) :: Inl(Inl(Inl(()))) :: HNil) :: Inl(Inl(())) :: HNil,
    l => println(s"⇒ $l".toLowerCase),
    r => println(s"⇒ _${r}_".toLowerCase))

  sealed trait Channel
  case object pub extends Channel
  case object coin extends Channel
  case object coffee extends Channel
  case object tea extends Channel
  case object chocs extends Channel
  case object figs extends Channel
  case object crisps extends Channel
  case object item extends Channel

  object Channel {
    implicit val readChannel: Read[Channel \/ Channel] =
      new Read[Channel \/ Channel] {
        def read(s: String): Option[Channel \/ Channel] = s match {
          case "pub" => pub.in.some
          case "coin" => coin.in.some
          case "coffee" => coffee.in.some
          case "tea" => tea.in.some
          case "chocs" => chocs.in.some
          case "figs" => figs.in.some
          case "crisps" => crisps.in.some
          case "_pub_" => pub.out.some
          case "_coin_" => coin.out.some
          case "_coffee_" => coffee.out.some
          case "_tea_" => tea.out.some
          case "_chocs_" => chocs.out.some
          case "_figs_" => figs.out.some
          case "_crisps_" => crisps.out.some
          case _ => Option.empty
        }
      }
  }
}
