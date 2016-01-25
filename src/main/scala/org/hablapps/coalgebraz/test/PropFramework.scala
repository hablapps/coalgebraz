package org.hablapps.coalgebraz.test

import org.hablapps.coalgebraz._, Driver._

object PropFramework {

  sealed trait Satisfied {

    def and(other: => Satisfied): Satisfied = (this, other) match {
      case (No, _) | (_, No) => No
      case (Yes, Yes) => Yes
      case _ => DontKnow
    }

    def or(other: => Satisfied): Satisfied = (this, other) match {
      case (Yes, _) | (_, Yes) => Yes
      case (No, No) => No
      case _ => DontKnow
    }
  }
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
        case Pred(f) => if (f(ht.current)) Yes else No
        case Next(f) => {
          ht.transition(input.head)._2.fold[Satisfied](No) { ht2 =>
            satisfiedHypertree(ht2)(f(ht2.current), input.tail, timeout - 1)
          }
        }
        case And(f1, f2) => {
          (satisfiedHypertree(ht)(f1(ht.current), input, timeout - 1)
            and satisfiedHypertree(ht)(f2(ht.current), input, timeout - 1))
        }
        case _ => ??? // TODO
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
