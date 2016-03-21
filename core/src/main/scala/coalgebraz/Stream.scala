package coalgebraz

import Function.const

import scalaz._, Scalaz._

import shapeless._, shapeless.{ :+:, Coproduct }

trait StreamCore extends syntax.ToStreamOps {

  type Stream[H, X] = Coalgebra[StreamF[H, ?], X]

  def stream[H, X](pi1: X => H, pi2: X => X): Stream[H, X] =
    x => StreamF(pi1(x), pi2(x))

  def repeatS[H, X](h: H): Stream[H, X] =
    stream(const(h), identity)

  def oddsS[H, X](s: Stream[H, X]): Stream[H, X] =
    stream(x => s(x).head, x => s(s(x).tail).tail)

  def evensS[H, X](s: Stream[H, X]): Stream[H, X] =
    stream(x => s(s(x).tail).head, x => s(s(x).tail).tail)

  def mergeS[H, X, Y](
      s1: Stream[H, X])(
      s2: Stream[H, Y]): Stream[H, (X, Y) :+: (Y, X) :+: CNil] = {
    type T = (X, Y) :+: (Y, X) :+: CNil
    object toStream extends Poly1 {
      implicit val caseXY = at[(X, Y)] { case (x, y) => 
        s1(x).map(Coproduct[T](y, _))
      }
      implicit val caseYX = at[(Y, X)] { case (y, x) => 
        s2(y).map(Coproduct[T](x, _)) 
      }
    }
    _.fold(toStream)
  }

  def untilS[H, X](
      s1: Stream[H, X],
      s2: Stream[H, X])(p: H => Boolean): Stream[H, (X, Boolean)] = {
    case (x, b) => {
      lazy val StreamF(h1, x1) = s1(x)
      lazy val StreamF(h2, x2) = s2(x)
      b.fold(StreamF(h2, (x2, true)), StreamF(h1, (x1, p(h1))))
    }
  }
}
