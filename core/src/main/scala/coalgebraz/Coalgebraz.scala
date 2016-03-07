package coalgebraz

object Coalgebraz extends CodataInstances
  with EntityCore with driver.EntityDriver
  with MealyCore with driver.MealyDriver
  with MilnerCore with driver.MilnerDriver
  with MooreCore with driver.MooreDriver
  with ObjectCore with driver.ObjectDriver
  with StreamCore with driver.StreamDriver
  with TransitionSystemCore with driver.TransitionSystemDriver
  with AdaptDsl
