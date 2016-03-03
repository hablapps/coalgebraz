package coalgebraz.driver

import scala.io.StdIn.readLine

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

trait ObjectDriver {

  def runObject[I, O, E, X](
      ob: Object[I, O, E, X])(x: X): NonEmptyList[I] => (E \/ O) =
    unfoldObject(ob)(x)

  def runIOObject[I: Read, O, E, X](
      ob: Object[I, O, E, X])(x: X, efe: E => Unit, efo: O => Unit): Unit =
    runFunctionIOObject(unfoldObject(ob)(x))(efe, efo)

  def runFunctionIOObject[I: Read, O, E](
      f: NonEmptyList[I] => (E \/ O))(
      efe: E => Unit,
      efo: O => Unit): Unit = {
    val s = readLine("z> ")
    val oi = Read[I].read(s)
    oi.fold(s match {
      case "" => runFunctionIOObject(f)(efe, efo)
      case "exit" => println("Bye!")
      case _ => {
        println(s"unknown input: '$s'")
        runFunctionIOObject(f)(efe, efo)
      }
    }) { i =>
      f(i.wrapNel).fold(
        efe,
        o => {
          efo(o)
          runFunctionIOObject(
            (i2: NonEmptyList[I]) => f(i.wrapNel append i2))(efe, efo)
        })
    }
  }

  def unfoldObject[I, O, E, X](
      ob: Object[I, O, E, X])(x: X): NonEmptyList[I] => (E \/ O) = {
    case NonEmptyList(i, INil()) => ob(x).method(i).map(_._1)
    case NonEmptyList(i1, ICons(i2, t)) => {
      ob(x).method(i1).fold(
        e  => e.left,
        ox => unfoldObject(ob)(ox._2)(NonEmptyList.nel(i2, t)))
    }
  }
}
