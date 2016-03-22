package coalgebraz
package example.basic

import shapeless._

import Coalgebraz._

object Stream extends App {

  val fibonacci: Stream[Long, (Long, Long)] =
    stream(_._1, { case (current, old) => (current + old, current)})

  runIO(fibonacci, 30)(
    (1, 0),
    h => println(s"⇒ $h"))
  println

  val nats: Stream[Int, Int] = stream(identity, _ + 1)

  def all = nats.odds merge nats.evens

  runIO(all)(Coproduct(1, 1), h => println(s"⇒ $h"))
  println

  def oddsXevens = nats.odds until (_ > 25, nats.evens)

  runIO(oddsXevens)(
    Coproduct((1, 1)),
    h => println(s"⇒ $h"))
}
