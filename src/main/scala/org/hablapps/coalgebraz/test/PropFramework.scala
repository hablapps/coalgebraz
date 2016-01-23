package org.hablapps.coalgebraz.test

import org.hablapps.coalgebraz._, Driver._

object PropFramework {

  sealed trait Satisfied
  case object Yes extends Satisfied
  case object No extends Satisfied
  case object DontKnow extends Satisfied

  def satisfiedHypertree[I, O, B](
      ht: Hypertree[I, O, B])(
      prop: EntityProp[B],
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
            satisfiedHypertree(ht2)(an.nxt(()), input.tail, timeout - 1)
          }
        }
        case _ => No
      }
    }
  }

  def satisfied[I, O, B, X](
      co: Entity[I, O, B, X])(
      prop: EntityProp[B],
      x0: X,
      input: List[I] = List.empty,
      timeout: Long = 100): Satisfied =
    satisfiedHypertree(unfold(co, x0))(prop, input, timeout)
}
