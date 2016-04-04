package coalgebraz
package driver

import Coalgebraz._

trait TransitionSystemDriver {

  def runTransitionSystemIO[X](
      ts: TransitionSystem[X])(
      x: X): Unit =
    runBareTreeIO(unfoldTS(ts)(x))

  def runBareTreeIO(bt: BareTree): Unit = {
    bt.branches.flatMap { bt2 =>
      if (bt2.branches.isEmpty)
        println("⇒ Dead branch")
      else
        println(s"⇒ Expanding '${bt2.branches.size}' new branches")
      bt2.branches
    } foreach(runBareTreeIO(_))
  }

  def unfoldTS[X](ts: TransitionSystem[X])(x: X): BareTree = anaTS(ts)(x)

  def anaTS[X](ts: TransitionSystem[X]): X => BareTree =
    ts andThen (_ map anaTS(ts)) andThen (tsf => BareTree(tsf.next))

  case class BareTree(branches: List[BareTree])
}
