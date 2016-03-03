package coalgebraz.driver

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

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

  def unfoldMealy[I, O, X](m: Mealy[I, O, X])(x: X): NonEmptyList[I] => O = {
    case NonEmptyList(i1, INil()) => m(x).next(i1)._1
    case NonEmptyList(i1, ICons(i2, is)) =>
      unfoldMealy(m)(m(x).next(i1)._2)(NonEmptyList(i2, is.toList: _*))
  }
}
