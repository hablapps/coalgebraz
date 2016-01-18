package org.hablapps.coalgebraz

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

object Driver {

  def behaviour[I, O, B, X](
      co: Coentity[I, O, B, X], x: X, in: List[I]): List[B] =
    run(co, x, in).map(_._2)

  def output[I, O, B, X](
      co: Coentity[I, O, B, X], x: X, in: List[I]): List[O] =
    run(co, x, in).map(_._1).flatten

  def run[I, O, B, X](
      co: Coentity[I, O, B, X], x: X, in: List[I]): List[(List[O], B)] =
    runHypertree(unfold(co, x), in)

  def runHypertree[I, O, B](
      ht: Hypertree[I, O, B],
      in: List[I]): List[(List[O], B)] = in match {
    case (i::is) => {
      val (os, ox) = ht.transition(i)
      val v = List((os, ht.current))
      ox.fold(v)(ht2 => v ++ runHypertree(ht2, is))
    }
    case Nil => List((List.empty, ht.current))
  }

  def runIO[I: Read, O, B, X](
      co: Coentity[I, O, B, X],
      x: X,
      eff: B => Unit): Unit =
    runHypertreeIO(unfold(co, x), eff)

  def runHypertreeIO[I: Read, O, B](
      ht: Hypertree[I, O, B],
      eff: B => Unit): Unit = {
    val s = readLine("$ ")
    val oi = Read[I].read(s)
    oi.fold({
      if (s == "exit")
        println("Bye")
      else if (s == "")
        runHypertreeIO(ht, eff)
      else {
        println(s"unknown input: '$s'")
        runHypertreeIO(ht, eff)
      }
    }) { i =>
      val (os, ox) = ht.transition(i)
      eff(ht.current)
      ox.fold(println("Bye!"))(ht2 => runHypertreeIO(ht2, eff))
    }
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
