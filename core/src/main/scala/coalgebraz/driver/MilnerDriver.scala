package coalgebraz
package driver

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

import Coalgebraz._

trait MilnerDriver {

  def runMilnerIO[A, X](
      m: Milner[A, X])(
      x: X)(implicit
      ev0: Read[A \/ A]): Unit =
    runDisjTreeIO(unfoldMilner(m)(x))

  def runDisjTreeIO[A](dt: DisjTree[A])(implicit ev0: Read[A \/ A]): Unit = {
    dt.branches.foreach { b =>
      println(s"""⇒ ${b._1.fold(in => in, out => "_" + out + "_")}""")
    }
    val s = readLine("z> ")
    val oa = Read[A \/ A].read(s)
    oa.fold(s match {
      case "" => runDisjTreeIO(dt)
      case "exit" => println("Bye!")
      case _ => {
        println(s"unknown input: '$s'")
        runDisjTreeIO(dt)
      }
    }) { a =>
      dt.branches.find(_._1 == a).fold {
        println(s"invalid transition: '$s'")
        runDisjTreeIO(dt)
      } { case (_, dt2) => runDisjTreeIO(dt2) }
    }
  }

  def unfoldMilner[A, X](m: Milner[A, X])(x: X): DisjTree[A] = anaMilner(m)(x)

  def anaMilner[A, X](m: Milner[A, X]): X => DisjTree[A] =
    m andThen (_ map anaMilner(m)) andThen (mf => DisjTree(mf.next))

  case class DisjTree[A](branches: List[(A \/ A, DisjTree[A])])
}
