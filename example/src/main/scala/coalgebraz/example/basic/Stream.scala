package coalgebraz.example.basic

import coalgebraz._, Coalgebraz._

object Stream extends App {

  val fibonacci: Stream[Long, (Long, Long)] =
    stream(_._1, { case (current, old) => (current + old, current)})

  runIO(fibonacci, 30)(
    (1, 0),
    h => println(s"⇒ $h"))
  println

  def odd[A]: Stream[A, List[A]] = stream(_.head, _.tail.tail)

  def even[A]: Stream[A, List[A]] = stream(_.tail.head, _.tail.tail)

  def all[A]: Stream[A, (List[A], List[A], Boolean)] = odd merge even

  runIO(all[Int])(
    (1 to 100 toList, 1 to 100 toList, true),
    h => println(s"⇒ $h"))
  println

  runIO(odd[Int].until(_ > 25, even))(
    (1 to 100 toList, false),
    h => println(s"⇒ $h"))
}
