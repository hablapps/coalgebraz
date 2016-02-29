package coalgebraz.example.basic

import coalgebraz._, Coalgebraz._

object Moore extends App {

  /* Odd-size recognition */

  def odd: Moore[Char, Boolean, Boolean] =
    moore(identity, x => _ => ! x)

  val f = run(odd)(false)
  assert(f("".toList) == false)
  assert(f("a".toList) == true)
  assert(f("ab".toList) == false)
  assert(f("abc".toList) == true)

  /* AAB recognition */

  trait QState
  case object Q0 extends QState
  case object Q1 extends QState
  case object Q2 extends QState
  case object Q3 extends QState

  trait Input
  case object A extends Input
  case object B extends Input

  def aab: Moore[Input, Boolean, QState] =
    moore(_ == Q3, q => i => (q, i) match {
      case (Q0, A) => Q1
      case (Q0, B) => Q0
      case (Q1, A) => Q2
      case (Q1, B) => Q0
      case (Q2, A) => Q2
      case (Q2, B) => Q3
      case (Q3, _) => Q3
    })

  val g = run(aab)(Q0)
  assert(g(List(A)) == false)
  assert(g(List(A, A)) == false)
  assert(g(List(A, A, B)) == true)
  assert(g(List(A, A, B, A)) == true)
  assert(g(List(A, B, A, B, A)) == false)
}
