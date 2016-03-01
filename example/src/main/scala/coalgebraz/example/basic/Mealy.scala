package coalgebraz.example.basic

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

object Mealy extends App {

  /* Odd-size recognition */

  def odd: Mealy[Alphabet, Boolean, Boolean] =
    mealy(x => i => (! x, ! x))

  val f = run(odd)(false)
  assert(f(NonEmptyList(A)) == true)
  assert(f(NonEmptyList(A, B)) == false)
  assert(f(NonEmptyList(A, B, A)) == true)
  assert(f(NonEmptyList(A, B, A, A)) == false)

  /* AAB recognition */

  def aab: Mealy[Alphabet, Boolean, QState] = mealy(x => i => (x, i) match {
    case (Q0, A) => (false, Q1)
    case (Q0, B) => (false, Q0)
    case (Q1, A) => (false, Q2)
    case (Q1, B) => (false, Q0)
    case (Q2, A) => (false, Q2)
    case (Q2, B) => (true,  Q0)
  })

  val g = run(aab)(Q0)
  assert(g(NonEmptyList(A)) == false)
  assert(g(NonEmptyList(A, A)) == false)
  assert(g(NonEmptyList(A, A, B)) == true)
  assert(g(NonEmptyList(A, A, B, A)) == false)
  assert(g(NonEmptyList(A, B, A, A, B)) == true)

  /* AAB + Odd-size recognition */

  val aabAndOdd: Mealy[Alphabet, Boolean, (Boolean, QState)] =
    (odd |*| aab)
      .in((i: Alphabet) => NonEmptyList((i, i)))
      .out(os => os._1 && os._2)

  runIO(aabAndOdd)((false, Q0), o => println(s"⇒ $o"))

  // (*) Notice that we're using one less state than `Moore.QState`
  sealed trait QState
  case object Q0 extends QState
  case object Q1 extends QState
  case object Q2 extends QState

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
