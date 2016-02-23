package coalgebraz

import Coalgebraz._

trait Mappable[F[_, _]] {

  def +[K, V](fa: F[K, V])(kv: (K, V)): F[K, V]

  def -[K, V](fa: F[K, V])(k: K): F[K, V]

  def get[K, V](fa: F[K, V])(k: K): Option[V]

  def toList[K, V](fa: F[K, V]): List[(K, V)]

  def filter[K, V](fa: F[K, V])(f: ((K, V)) => Boolean): F[K, V]

  def contains[K, V](fa: F[K, V])(k: K): Boolean = get[K, V](fa)(k).isDefined
}

object Mappable {

  implicit def mappableMap = new Mappable[Map] {

    def +[K, V](fa: Map[K, V])(kv: (K, V)): Map[K, V] = fa + kv

    def -[K, V](fa: Map[K, V])(k: K): Map[K, V] = fa - k

    def get[K, V](fa: Map[K, V])(k: K): Option[V] = fa get k

    def filter[K, V](fa: Map[K, V])(f: ((K, V)) => Boolean): Map[K, V] =
      fa filter f

    def toList[K, V](fa: Map[K, V]): List[(K, V)] = fa.toList
  }
}

class MappableOps[F[_, _], K, V](val self: F[K, V])(implicit M: Mappable[F]) {

  def +(kv: (K, V)): F[K, V] = M.+(self)(kv)

  def -(k: K): F[K, V] = M.-(self)(k)

  def get(k: K): Option[V] = M.get(self)(k)

  def contains(k: K): Boolean = M.contains(self)(k)

  def filter(f: ((K, V)) => Boolean) = M.filter(self)(f)

  def toList: List[(K, V)] = M.toList(self)
}

trait ToMappableOps {
  implicit def toMappableOps[F[_, _], K, V](v: F[K, V])(implicit
      M: Mappable[F]) =
    new MappableOps[F, K, V](v)(M)
}
