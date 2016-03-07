package coalgebraz.driver

import scala.collection.immutable.{ Stream => LazyList }

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

trait StreamDriver {

  def run[H, X](s: Stream[H, X], b: Int = 50)(x: X): LazyList[H] =
    unfoldStream(s)(x).take(b)

  def runIO[H, X](
      s: Stream[H, X],
      b: Int = 50)(
      x: X,
      e: H => Unit): Unit =
    run(s, b)(x).foreach(e)

  def unfoldStream[H, X](s: Stream[H, X])(x: X): LazyList[H] = anaStream(s)(x)

  def anaStream[H, X](s: Stream[H, X]): X => LazyList[H] =
    s andThen (_ map anaStream(s)) andThen (sf => sf.head #:: sf.tail)
}
