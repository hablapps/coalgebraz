package coalgebraz

trait Ordered[A] {

  def compare(a1: A, a2: A): Int

  def min: A

  def max: A

  def lt(a1: A, a2: A): Boolean = compare(a1, a2) < 0

  def let(a1: A, a2: A): Boolean = compare(a1, a2) <= 0

  def gt(a1: A, a2: A): Boolean = compare(a1, a2) > 0

  def get(a1: A, a2: A): Boolean = compare(a1, a2) >= 0
}

object Ordered {
  def apply[A](implicit ev: Ordered[A]): Ordered[A] = ev
}

class OrderedOps[A](val self: A)(implicit O: Ordered[A]) {

  def <(that: A): Boolean = O.lt(self, that)

  def <=(that: A): Boolean = O.let(self, that)

  def >(that: A): Boolean = O.gt(self, that)

  def >=(that: A): Boolean = O.get(self, that)
}

trait ToOrderedOps {
  implicit def toOrderedOps[A](v: A)(implicit O: Ordered[A]) =
    new OrderedOps(v)(O)
}
