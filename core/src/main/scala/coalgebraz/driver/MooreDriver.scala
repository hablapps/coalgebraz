package coalgebraz.driver

import scala.io.StdIn.readLine

import coalgebraz._, Coalgebraz._

trait MooreDriver {

  def run[I, O, X](m: Moore[I, O, X])(x: X): List[I] => O = unfold(m)(x)

  def runIO[I: Read, O, X](m: Moore[I, O, X])(x: X, e: O => Unit): Unit =
    runFunctionIO(unfold(m)(x))(e)

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

  def unfold[I, O, X](m: Moore[I, O, X])(x: X): List[I] => O = {
    val MooreF(o, nxt) = m(x)
    s => if (s.isEmpty) o else unfold(m)(nxt(s.head))(s.tail)
  }
}
