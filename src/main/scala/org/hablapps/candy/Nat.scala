package org.hablapps.candy

case class Nat(us: List[Unit]) {
  def asInt: Int = us.length
  override def toString = asInt.toString
}

object Nat {

  // If input is negative, the generated value will be set to zero.
  def apply(i: Int): Nat = Nat(List.fill(i)(()))

  def zero: Nat = Nat(List.empty)

  def add(a: Nat, b: Nat): Nat = Nat(a.us ++ b.us)

  def sub(a: Nat, b: Nat): Nat = apply(a.asInt - b.asInt)

  def lt(a: Nat, b: Nat): Boolean = a.asInt < b.asInt

  def ht(a: Nat, b: Nat): Boolean = a.asInt > b.asInt

  case class NatOps(a: Nat) {
    def +(b: Nat) = Nat.add(a, b)
    def -(b: Nat) = Nat.sub(a, b)
    def <(b: Nat) = Nat.lt(a, b)
    def >(b: Nat) = Nat.ht(a, b)
  }

  implicit def intToNat(i: Int): Nat = apply(i)

  implicit def natToInt(n: Nat): Int = n.asInt

  object Syntax {
    implicit def toNatOps(a: Nat): NatOps = NatOps(a)
  }
}
