package coalgebraz
package driver

import scala.io.StdIn.readLine

import Coalgebraz._

trait MooreDriver {

  def run[I, O, X](m: Moore[I, O, X])(x: X): List[I] => O = unfoldMoore(m)(x)

  def runIO[I: Read, O, X](m: Moore[I, O, X])(x: X, e: O => Unit): Unit =
    runFunctionIO(unfoldMoore(m)(x))(e)

  def runFunctionIO[I: Read, O](
      f: List[I] => O)(
      e: O => Unit): Unit = {
    val s = readLine("z> ")
    val oi = Read[I].read(s)
    oi.fold(s match {
      case "" => runFunctionIO(f)(e)
      case "exit" => println("Bye!")
      case _ => {
        println(s"unknown input: '$s'")
        runFunctionIO(f)(e)
      }
    }) { i =>
      e(f(List(i)))
      runFunctionIO((i2: List[I]) => f(List(i) ++ i2))(e)
    }
  }

  def unfoldMoore[I, O, X](m: Moore[I, O, X])(x: X): List[I] => O =
    anaMoore(m)(x)

  def anaMoore[I, O, X](s: Moore[I, O, X]): X => List[I] => O =
    s andThen (_ map anaMoore(s)) andThen (mf => xs =>
      if (xs.isEmpty) mf.output else mf.next(xs.head)(xs.tail))
}
