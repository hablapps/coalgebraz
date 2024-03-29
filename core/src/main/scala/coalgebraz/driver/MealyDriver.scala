package coalgebraz
package driver

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

import Coalgebraz._

trait MealyDriver {

  def runMealy[I, O, X](m: Mealy[I, O, X])(x: X): NonEmptyList[I] => O =
    unfoldMealy(m)(x)

  def runIOMealy[I: Read, O, X](m: Mealy[I, O, X])(x: X, e: O => Unit): Unit =
    runNelFunctionIO(unfoldMealy(m)(x))(e)

  def runNelFunctionIO[I: Read, O](
      f: NonEmptyList[I] => O)(
      e: O => Unit): Unit = {
    val s = readLine("z> ")
    val oi = Read[I].read(s)
    oi.fold(s match {
      case "" => runNelFunctionIO(f)(e)
      case "exit" => println("Bye!")
      case _ => {
        println(s"unknown input: '$s'")
        runNelFunctionIO(f)(e)
      }
    }) { i =>
      e(f(i.wrapNel))
      runNelFunctionIO((i2: NonEmptyList[I]) => f(i.wrapNel append i2))(e)
    }
  }

  def unfoldMealy[I, O, X](m: Mealy[I, O, X])(x: X): NonEmptyList[I] => O =
    anaMealy(m)(x)

  def anaMealy[I, O, X](m: Mealy[I, O, X]): X => NonEmptyList[I] => O =
    m andThen (_ map anaMealy(m)) andThen (mf => {
      case NonEmptyList(x, INil()) => mf.next(x)._1
      case NonEmptyList(x1, ICons(x2, xs)) =>
        mf.next(x1)._2(NonEmptyList.nel(x2, xs))
    })
}
