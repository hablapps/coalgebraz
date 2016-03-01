package coalgebraz

trait MooreCore extends ToMooreOps {

  def moore[I, O, X](pi1: X => O, pi2: X => I => X): Moore[I, O, X] =
    x => MooreF(pi1(x), pi2(x))

  // TODO: Moore combinators??? product, coproduct, andThen...
}
