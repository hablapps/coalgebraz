package coalgebraz.example.basic

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

object Moore extends App {

  /* Odd-size recognition */

  def odd: Moore[Alphabet, Boolean, Boolean] =
    moore(identity, x => _ => ! x)

  val f = run(odd)(false)
  assert(f(List()) == false)
  assert(f(List(A)) == true)
  assert(f(List(A, B)) == false)
  assert(f(List(A, B, A)) == true)

  /* AAB recognition */

  def aab: Moore[Alphabet, Boolean, QState] =
    moore(_ == Q3, q => i => (q, i) match {
      case (Q0, A) => Q1
      case (Q0, B) => Q0
      case (Q1, A) => Q2
      case (Q1, B) => Q0
      case (Q2, A) => Q2
      case (Q2, B) => Q3
      case (Q3, A) => Q1
      case (Q3, B) => Q0
    })

  val g = run(aab)(Q0)
  assert(g(List(A)) == false)
  assert(g(List(A, A)) == false)
  assert(g(List(A, A, B)) == true)
  assert(g(List(A, A, B, A)) == false)
  assert(g(List(A, B, A, A, B)) == true)

  /* AAB + Odd-size recognition */

  val aabOrOdd: Moore[Alphabet, Boolean, (Boolean, QState)] =
    (odd |*| aab)
      .in((i: Alphabet) => List(i.left, i.right))
      .out(os => os._1 || os._2)

  runIO(aabOrOdd)((false, Q0), o => println(s"⇒ $o"))

  sealed trait QState
  case object Q0 extends QState
  case object Q1 extends QState
  case object Q2 extends QState
  case object Q3 extends QState

  sealed trait Alphabet
  case object A extends Alphabet
  case object B extends Alphabet

  object Alphabet {
    implicit val readAlphabet: Read[Alphabet] = new Read[Alphabet] {
      def read(s: String): Option[Alphabet] = s match {
        case "a" | "A" => Option(A)
        case "b" | "B" => Option(B)
        case _ => None
      }
    }
  }
}
