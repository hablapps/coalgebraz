package coalgebraz
package driver

import scala.collection.immutable.{ Stream => LazyList }

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

import Coalgebraz._

trait MilnerDriver {

  def runMilnerIO[A, X](
      m: Milner[A, X])(
      x: X,
      el: A => Unit,
      er: A => Unit)(implicit
      ev0: Read[A \/ A]): Unit =
    runDisjTreeIO(unfoldMilner(m)(x))(el, er)

  def runDisjTreeIO[A](
      dt: DisjTree[A])(
      el: A => Unit, er: A => Unit)(implicit
      ev0: Read[A \/ A]): Unit = {
    dt.branches.foreach(b => b._1.fold(el, er))
    val s = readLine("z> ")
    val oa = Read[A \/ A].read(s)
    oa.fold(s match {
      case "" => runDisjTreeIO(dt)(el, er)
      case "exit" => println("Bye!")
      case _ => {
        println(s"unknown input: '$s'")
        runDisjTreeIO(dt)(el, er)
      }
    }) { a =>
      dt.branches.find(_._1 == a).fold {
        println(s"invalid transition: '$s'")
        runDisjTreeIO(dt)(el, er)
      } { case (_, dt2) => runDisjTreeIO(dt2)(el, er) }
    }
  }

  def unfoldMilner[A, X](m: Milner[A, X])(x: X): DisjTree[A] = anaMilner(m)(x)

  def anaMilner[A, X](m: Milner[A, X]): X => DisjTree[A] =
    m andThen (_ map anaMilner(m)) andThen (mf => DisjTree(mf.next))

  class DisjTree[A](_branches: => LazyList[(A \/ A, DisjTree[A])]) {
    def branches: LazyList[(A \/ A, DisjTree[A])] = _branches
  }

  object DisjTree {
    def apply[A](branches: => LazyList[(A \/ A, DisjTree[A])]): DisjTree[A] =
      new DisjTree(branches)
  }
}
