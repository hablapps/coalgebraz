package coalgebraz.example.basic

import coalgebraz._, Coalgebraz._

object Stream extends App {

  val sumLastTwo: Stream[Long, (Long, Long)] =
    stream(_._1, { case (current, old) => (current + old, current)})

  val fibonacci = run(sumLastTwo)(1, 0)

  def odd[A]: Stream[A, List[A]] = stream(_.head, _.tail.tail)

  def even[A]: Stream[A, List[A]] = stream(_.tail.head, _.tail.tail)

  def all[A]: Stream[A, (List[A], List[A], Boolean)] = odd merge even

  runIO(all[Int])((1 to 500 toList, 1 to 500 toList, true), println(_))

  runIO(odd[Int].until(_ > 50, even))((1 to 500 toList, false), println(_))
}
