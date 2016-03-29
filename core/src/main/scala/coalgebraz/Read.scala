package coalgebraz

import scala.util.matching.Regex

import scalaz._, Scalaz._

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

  implicit val readString = new Read[String] {
    def read(s: String) = {
      val pattern = """\"(.*)\"""".r
      s match {
        case pattern(v) => Option(v)
        case _ => None
      }
    }
  }

  implicit val readChar = new Read[Char] {
    def read(s: String) = s match {
      case s if s.length == 1 => s.head.some
      case _ => Option.empty
    }
  }
}
