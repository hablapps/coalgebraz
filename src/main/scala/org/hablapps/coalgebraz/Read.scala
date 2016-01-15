package org.hablapps.coalgebraz

trait Read[A] {
  def read(s: String): Option[A]
}

object Read {
  def apply[A](implicit R: Read[A]): Read[A] = R
}
