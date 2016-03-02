package coalgebraz.example.basic

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

object Object extends App {

  def stringBuffer: Object[String, String, Error, String] =
    obj(s1 => s2 => (s1 ++ s2, s1 ++ s2).right)

  def bound(b: Int): Object[String, String, Error, Unit] =
    obj(_ => s => if (s.length > b) OutOfBound.left else (s, ()).right)

  def boundedStringBuffer(b: Int): Object[String, String, Error, String] =
    stringBuffer |=>| bound(25)

  runIOObject(boundedStringBuffer(10))(
    "", e => println(s"⤃ $e"), o => println(s"⇒ $o"))

  sealed trait Error
  case object OutOfBound extends Error
}
