package org.hablapps.coalgebraz.test

import org.hablapps.coalgebraz._, Driver._

object PropFramework {

  sealed trait Satisfied
  case object Yes extends Satisfied
  case object No extends Satisfied
  case object DontKnow extends Satisfied

  def satisfiedHypertree[I, O, B](
      prop: EntityProp[B],
      ht: Hypertree[I, O, B],
      input: List[I] = List.empty,
      timeout: Long = 100): Satisfied = {
    if (timeout == 0 || input.isEmpty) {
      DontKnow
    } else {
      prop match {
        case Now(f) if f(ht.current) => Yes
        case Now(_) => No
        case an: AndNext[B] if an.now.f(ht.current) => {
          val (_, ox) = ht.transition(input.head)
          ox.fold[Satisfied](Yes) { ht2 =>
            satisfiedHypertree(an.nxt(()), ht2, input.tail, timeout - 1)
          }
        }
        case _ => No
      }
    }
  }

  def satisfied[I, O, B, X](
      prop: EntityProp[B],
      co: Entity[I, O, B, X],
      x0: X,
      input: List[I] = List.empty,
      timeout: Long = 100): Satisfied =
    satisfiedHypertree(prop, unfold(co, x0), input, timeout)
}
