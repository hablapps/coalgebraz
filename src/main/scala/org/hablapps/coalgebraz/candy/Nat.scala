package org.hablapps.coalgebraz.candy

import scala.language.implicitConversions

case class Nat(us: List[Unit])

object Nat {

  // If input is negative, the generated value will be set to zero.
  def apply(i: Int): Nat = Nat(List.fill(i)(()))

  def zero: Nat = Nat(List.empty)

  def add(a: Nat, b: Nat): Nat = Nat(a.us ++ b.us)

  def sub(a: Nat, b: Nat): Nat = apply(toInt(a)-toInt(b))

  def toInt(a: Nat): Int = a.us.length

  case class NatOps(a: Nat) {
    def +(b: Nat): Nat = Nat.add(a, b)
    def -(b: Nat): Nat = Nat.sub(a, b)
    def asInt = Nat.toInt(a)
  }

  object Syntax {
    implicit def toNatOps(a: Nat): NatOps = NatOps(a)
  }
}
