package coalgebraz

trait MealyCore extends ToMealyOps {

  type Mealy[I, O, X] = Coalgebra[MealyF[I, O, ?], X]
}
