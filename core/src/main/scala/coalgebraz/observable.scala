package coalgebraz

trait Observable[B, A] {
  def observe(a: A): B
}

object Observable {
  implicit def identityObservable[A] = new Observable[A, A] {
    def observe(a: A): A = a
  }
}

class ObservableOps[B, A](val a: A)(implicit O: Observable[B, A]) {
  def observe: B = O.observe(a)
}

trait ToObservableOps {
  implicit def toObservableOps[B, A](a: A)(implicit I: Observable[B, A]) =
    new ObservableOps[B, A](a)(I)
}
