package coalgebraz
package driver

import scala.collection.immutable.{ Stream => LazyList }

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

import Coalgebraz._

trait CCSDriver {

  def runCCSIO[A, X](
      m: CCS[A, X])(
      x: X,
      el: A => Unit,
      er: A => Unit)(implicit
      ev0: Read[A \/ A]): Unit =
    runLTSIO(unfoldCCS(m)(x))(el, er)

  def runLTSIO[A](
      dt: LTS[A])(
      el: A => Unit, er: A => Unit)(implicit
      ev0: Read[A \/ A]): Unit = {
    if (dt.branches.isEmpty)
      println("deadlocked!")
    else {
      dt.branches.foreach(b => b._1.fold(el, er))
      val s = readLine("z> ")
      val oa = Read[A \/ A].read(s)
      oa.fold(s match {
        case "" => runLTSIO(dt)(el, er)
        case "exit" => println("bye!")
        case _ => {
          println(s"unknown input: '$s'")
          runLTSIO(dt)(el, er)
        }
      }) { a =>
        dt.branches.find(_._1 == a).fold {
          println(s"invalid transition: '$s'")
          runLTSIO(dt)(el, er)
        } { case (_, dt2) => runLTSIO(dt2)(el, er) }
      }
    }
  }

  def unfoldCCS[A, X](m: CCS[A, X])(x: X): LTS[A] = anaCCS(m)(x)

  def anaCCS[A, X](m: CCS[A, X]): X => LTS[A] =
    m andThen (_ map anaCCS(m)) andThen (mf => LTS(mf.next))

  class LTS[A](_branches: => LazyList[(A \/ A, LTS[A])]) {
    def branches: LazyList[(A \/ A, LTS[A])] = _branches
  }

  object LTS {
    def apply[A](branches: => LazyList[(A \/ A, LTS[A])]): LTS[A] =
      new LTS(branches)
  }
}
