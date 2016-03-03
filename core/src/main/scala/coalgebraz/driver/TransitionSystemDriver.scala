package coalgebraz.driver

import coalgebraz._, Coalgebraz._

trait TransitionSystemDriver {

  def runTransitionSystemIO[X](
      ts: TransitionSystem[X])(
      x: X): Unit =
    runBareTreeIO(unfold(ts)(x))

  def runBareTreeIO(bt: BareTree): Unit = {
    bt.branches.flatMap { bt2 =>
      if (bt2.branches.isEmpty)
        println("⇒ Dead branch")
      else
        println(s"⇒ Expanding '${bt2.branches.size}' new branches")
      bt2.branches
    } foreach(runBareTreeIO(_))
  }

  def unfold[X](ts: TransitionSystem[X])(x: X): BareTree =
    BareTree(ts(x).next map (x2 => unfold(ts)(x2)))

  case class BareTree(branches: List[BareTree])
}
