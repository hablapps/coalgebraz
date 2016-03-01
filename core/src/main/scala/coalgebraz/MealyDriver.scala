package coalgebraz

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

import Coalgebraz._

trait MealyDriver {

  def run[I, O, X](m: Mealy[I, O, X])(x: X): NonEmptyList[I] => O = unfold(m)(x)

  def runIO[I: Read, O, X](m: Mealy[I, O, X])(x: X, e: O => Unit): Unit =
    runNelFunctionIO(unfold(m)(x))(e)

  def runNelFunctionIO[I: Read, O](
      f: NonEmptyList[I] => O)(
      e: O => Unit): Unit = {
    val s = readLine("ζ ")
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

  def unfold[I, O, X](m: Mealy[I, O, X])(x: X): NonEmptyList[I] => O = {
    case NonEmptyList(i1, INil()) => m(x).next(i1)._1
    case NonEmptyList(i1, ICons(i2, is)) =>
      unfold(m)(m(x).next(i1)._2)(NonEmptyList(i2, is.toList: _*))
  }
}
