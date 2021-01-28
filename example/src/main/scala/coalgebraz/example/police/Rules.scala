package com.hablapps.police
package akka

object Rules {
  type Rule = State => List[OutputEvent]
  val MAX_ENTITIES_PER_FENCE = 1

  val tooManyEntitiesInFence: Rule = state => {
    state.belongings.groupBy(_.geo).toList filter { _._2.length > MAX_ENTITIES_PER_FENCE } map {
      case (geo, xs) => TooManyEntitiesInFence(geo, xs.map(_.entity))
    }
  }

  val emptyFence: Rule = state => {
    state.fences diff state.belongings.map(_.geo) map EmptyFence.apply
  }

  val all: List[Rule] =
    tooManyEntitiesInFence ::
    emptyFence ::
    Nil
}
