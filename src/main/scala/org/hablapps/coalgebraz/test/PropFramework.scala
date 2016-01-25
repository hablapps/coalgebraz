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

    def negate: Satisfied = this match {
      case Yes => No
      case No => Yes
      case _ => DontKnow
    }
  }
  case object Yes extends Satisfied
  case object No extends Satisfied
  case object DontKnow extends Satisfied

  def satisfiedHypertree[I, O, B](
      ht: Hypertree[I, O, B])(
      prop: EntityProp[B],
      input: List[I] = List.empty): Satisfied = {
    if (input.isEmpty) { // kind of timeout
      DontKnow
    } else {
      prop match {
        case Pred(f) => if (f(ht.current)) Yes else No
        case Next(f) => {
          ht.transition(input.head)._2.fold[Satisfied](No) { ht2 =>
            satisfiedHypertree(ht2)(f(ht2.current), input.tail)
          }
        }
        case And(f1, f2) => {
          (satisfiedHypertree(ht)(f1(ht.current), input)
            and satisfiedHypertree(ht)(f2(ht.current), input))
        }
        case Not(f) =>
          satisfiedHypertree(ht)(f(ht.current), input).negate
      }
    }
  }

  def satisfied[I, O, B, X](
      co: Entity[I, O, B, X])(
      prop: EntityProp[B],
      x0: X,
      input: List[I] = List.empty): Satisfied =
    satisfiedHypertree(unfold(co, x0))(prop, input)
}
