package org.hablapps.coalgebraz

trait Read[A] {
  def read(s: String): Option[A]
}

object Read {

  def apply[A](implicit R: Read[A]): Read[A] = R

  implicit val readInt = new Read[Int] {
    def read(s: String) = try {
      Option(s.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }
}
