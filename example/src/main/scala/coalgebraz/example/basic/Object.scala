package coalgebraz.example.basic

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

object Object extends App {

  def stringBuffer: Object[String, String, Void, String] =
    obj(s1 => s2 => dup(s1 ++ s2).right)

  def bound(b: Int): Object[String, String, Error, Unit] =
    obj(_ => s => (s.length > b).fold(OutOfBounds.left, (s, ()).right))

  def boundedStringBuffer(b: Int): Object[String, String, Error, String] =
    stringBuffer |=>| bound(b)

  runIOObject(boundedStringBuffer(15))(
    "", e => println(s"⇏ $e"), o => println(s"⇒ $o"))

  sealed trait Error
  case object OutOfBounds extends Error

  def dup[A](a: A): (A, A) = (a, a)
}
