package org.hablapps.coalgebraz

import scalaz._, Scalaz._

trait ClearProduct[A, B] {
  type C
  def apply(a: A, b: B): C
}

trait ClearProductLowestPriority {
  implicit def tupleClear[A, B]: ClearProduct.Aux[A, B, (A, B)] =
    new ClearProduct[A, B] {
      type C = (A, B)
      def apply(a: A, b: B) = (a, b)
    }
}

trait ClearProductLowerPriority extends ClearProductLowestPriority {
  implicit def unit2Clear[B]: ClearProduct.Aux[Unit, B, B] =
    new ClearProduct[Unit, B] {
      type C = B
      def apply(a: Unit, b: B) = b
    }
}

object ClearProduct extends ClearProductLowerPriority {

  type Aux[A, B, C2] = ClearProduct[A, B] { type C = C2 }

  implicit def unit1Clear[A]: ClearProduct.Aux[A, Unit, A] =
    new ClearProduct[A, Unit] {
      type C = A
      def apply(a: A, b: Unit) = a
    }
}

trait ClearSum[A, B] {
  type C
  def apply(v: A \/ B): C
  def apply[D](fa: A => D, fb: B => D, v: C): D
}

trait ClearSumLowestPriority {
  implicit def disjClear[A, B]: ClearSum.Aux[A, B, A \/ B] =
    new ClearSum[A, B] {
      type C = A \/ B
      def apply(v: A \/ B) = v
      def apply[D](fa: A => D, fb: B => D, v: C) = v.fold(fa(_), fb(_))
    }
}

trait ClearSumLowerPriority extends ClearSumLowestPriority {
  implicit def void2Clear[B]: ClearSum.Aux[Void, B, B] =
    new ClearSum[Void, B] {
      type C = B
      def apply(v: Void \/ B) = v | ???
      def apply[D](fa: Void => D, fb: B => D, v: C) = fb(v)
    }
}

object ClearSum extends ClearSumLowerPriority {

  type Aux[A, B, C2] = ClearSum[A, B] { type C = C2 }

  implicit def void1Clear[A]: ClearSum.Aux[A, Void, A] =
    new ClearSum[A, Void] {
      type C = A
      def apply(v: A \/ Void) = v.swap | ???
      def apply[D](fa: A => D, fb: Void => D, v: C) = fa(v)
    }
}
