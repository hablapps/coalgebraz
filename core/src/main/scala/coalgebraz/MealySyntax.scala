package coalgebraz

import Coalgebraz._

class MealyOps[I1, O1, X1](val self: Mealy[I1, O1, X1]) {

}

trait ToMealyOps {
  implicit def toMealyOps[I, O, X](m: Mealy[I, O, X]): MealyOps[I, O, X] =
    new MealyOps[I, O, X](m)
}
