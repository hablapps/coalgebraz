package coalgebraz

import scalaz._, Scalaz._

trait ClearProduct[A, B] {
  type C
  def apply(a: A, b: B): C
  def piA(c: C): A
  def piB(c: C): B
}

object ClearProduct extends ClearProductLowPriorityImplicits1 {

  type Aux[A, B, C2] = ClearProduct[A, B] { type C = C2 }

  implicit def unit1Clear[A]: ClearProduct.Aux[A, Unit, A] =
    new ClearProduct[A, Unit] {
      type C = A
      def apply(a: A, b: Unit) = a
      def piA(c: C) = c
      def piB(c: C) = ()
    }
}

trait ClearProductLowPriorityImplicits1
    extends ClearProductLowPriorityImplicits0 {

  implicit def unit2Clear[B]: ClearProduct.Aux[Unit, B, B] =
    new ClearProduct[Unit, B] {
      type C = B
      def apply(a: Unit, b: B) = b
      def piA(c: C) = ()
      def piB(c: C) = c
    }
}

trait ClearProductLowPriorityImplicits0 {
  implicit def tupleClear[A, B]: ClearProduct.Aux[A, B, (A, B)] =
    new ClearProduct[A, B] {
      type C = (A, B)
      def apply(a: A, b: B) = (a, b)
      def piA(c: C) = c._1
      def piB(c: C) = c._2
    }
}

trait ClearSum[A, B] {
  type C
  def apply(v: A \/ B): C
  def apply[D](fa: A => D, fb: B => D, v: C): D
}

object ClearSum extends ClearSumLowPriorityImplicits1 {

  type Aux[A, B, C2] = ClearSum[A, B] { type C = C2 }

  implicit def void1Clear[A]: ClearSum.Aux[A, Void, A] =
    new ClearSum[A, Void] {
      type C = A
      def apply(v: A \/ Void) = v.swap | ???
      def apply[D](fa: A => D, fb: Void => D, v: C) = fa(v)
    }
}

trait ClearSumLowPriorityImplicits1 extends ClearSumLowPriorityImplicits0 {
  implicit def void2Clear[B]: ClearSum.Aux[Void, B, B] =
    new ClearSum[Void, B] {
      type C = B
      def apply(v: Void \/ B) = v | ???
      def apply[D](fa: Void => D, fb: B => D, v: C) = fb(v)
    }
}

trait ClearSumLowPriorityImplicits0 {
  implicit def disjClear[A, B]: ClearSum.Aux[A, B, A \/ B] =
    new ClearSum[A, B] {
      type C = A \/ B
      def apply(v: A \/ B) = v
      def apply[D](fa: A => D, fb: B => D, v: C) = v.fold(fa(_), fb(_))
    }
}
