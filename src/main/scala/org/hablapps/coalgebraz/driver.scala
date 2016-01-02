package org.hablapps.coalgebraz

import scalaz._, Scalaz._

object Driver {

  def runCoentity[I, O, B, X](
      co: Coentity[I, O, B, X], x: X, in: List[I]): List[(List[O], B)] =
    runHypertree(unfold(co, x), in)

  def runHypertree[I, O, B](
      ht: Hypertree[I, O, B], in: List[I]): List[(List[O], B)] = in match {
    case (i::is) => {
      val (os, ox) = ht.transition(i)
      val v = List((os, ht.current))
      ox.fold(v)(ht2 => v ++ runHypertree(ht2, is))
    }
    case Nil => List.empty
  }

  case class Hypertree[A, B, C](
    current: C,
    transition: A => (List[B], Option[Hypertree[A, B, C]]))

  def unfold[I, O, B, X](
      co: Coentity[I, O, B, X], x: X): Hypertree[I, O, B] = {
    val Entity(obs, nxt) = co(x)
    Hypertree(obs, i => nxt(i).map(_.map(unfold(co, _))))
  }
}
