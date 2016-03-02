package coalgebraz.example.basic

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

object Object extends App {

  def stringBuffer: Object[String, String, Void, String] =
    obj(s => i => (s ++ i, s ++ i).right)

  runIOObject(stringBuffer)("", e => ???, o => println(s"⇒ $o"))
}
