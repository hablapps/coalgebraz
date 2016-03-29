package coalgebraz
package example.basic

import scalaz._, Scalaz._

import shapeless._, shapeless.{ :+:, Coproduct }

import Coalgebraz._

object Entity extends App {

  val counter: Entity[Int, String, Int, Int] =
    next(x => i => (x + i) ~> s"Added $i")

  def copy[A]: Entity[A, A, Unit, Unit] =
    next(_ => i => () ~> i)

  // External choice
  def counterOrCopy: Entity[
      Int :+: Char :+: CNil,
      String :+: Char :+: CNil,
      (Int, Unit) :+: Int :+: Unit :+: CNil,
      (Int, Unit) :+: Int :+: Unit :+: CNil] =
    counter |#| copy[Char]

  runIO(counterOrCopy)(
    Coproduct[(Int, Unit) :+: Int :+: Unit :+: CNil](1, ()),
    println(_),
    _.foreach(o => println(s"=> $o")))

  implicit def readCoproduct: Read[Int :+: Char :+: CNil] = {
    type R = Int :+: Char :+: CNil
    new Read[R] {
      def read(s: String): Option[R] = s match {
        case s if Read[Int].read(s).isDefined =>
          Read[Int].read(s).map(Coproduct[R](_))
        case s if Read[Char].read(s).isDefined =>
          Read[Char].read(s).map(Coproduct[R](_))
        case _ => Option.empty
      }
    }
  }
}
