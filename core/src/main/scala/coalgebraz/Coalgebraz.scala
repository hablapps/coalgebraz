package coalgebraz

object Coalgebraz extends CodataInstances
  with CCSCore with driver.CCSDriver
  with EntityCore with driver.EntityDriver
  with MealyCore with driver.MealyDriver
  with MooreCore with driver.MooreDriver
  with ObjectCore with driver.ObjectDriver
  with StreamCore with driver.StreamDriver
  with TransitionSystemCore with driver.TransitionSystemDriver
  with AdaptDsl
