package coalgebraz
package syntax

import scalaz._, Scalaz._

import shapeless._, shapeless.{ :+:, Coproduct }
import ops.coproduct._, ops.coproduct.{ Inject }

import Coalgebraz._

class CCSOps[A1, X1](val self: CCS[A1, X1]) {

  def +[X2](m2: CCS[A1, X2]): CCS[A1, (X1, X2) :+: X1 :+: X2 :+: CNil] =
    choice(self)(m2)

  def |[X2](m2: CCS[A1, X2]): CCS[A1, X1 :: X2 :: HNil] =
    parallel(self)(m2)

  def \(r: Set[A1]): CCS[A1, X1] = restrict(self)(r)

  def /(f: PartialFunction[A1, A1]): CCS[A1, X1] = {
    def lift(f: A1 => A1): A1 \/ A1 => A1 \/ A1 = {
      case -\/(a) => -\/(f(a))
      case \/-(a) => \/-(f(a))
    }
    renaming(self)(lift(f orElse PartialFunction(identity)))
  }
}

trait ToCCSOps {

  implicit def toCCSOps[A, X](m: CCS[A, X]): CCSOps[A, X] =
    new CCSOps[A, X](m)

  implicit class InOutHelper[A](a: A) {
    def in: A \/ A = a.left
    def out: A \/ A = a.right
  }

  implicit class PrefixHelper[A, X](m: => CCS[A, X]) {
    def %:(a: A \/ A): CCS[A, X :+: X :+: CNil] = action[A, X](m)(a)
  }
}
