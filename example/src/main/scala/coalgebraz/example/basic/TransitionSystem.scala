package coalgebraz
package example.basic

import scalaz._, Scalaz._

import Coalgebraz._

object TransitionSystem extends App {

  def system: TransitionSystem[Int] =
    transition(i => if (i < 1 || i > 10) Nil else List(i+1, i*2))

  runTransitionSystemIO(system)(1)
}
