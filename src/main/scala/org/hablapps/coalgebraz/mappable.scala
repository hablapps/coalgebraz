package org.hablapps.coalgebraz

trait Mappable[F[_]] {

  def +[I, A: Indexable[I, ?]](fa: F[A])(a: A): F[A]

  def -[I, A: Indexable[I, ?]](fa: F[A])(i: I): F[A]

  def get[I, A: Indexable[I, ?]](fa: F[A])(i: I): Option[A]

  // XXX: weak link, we are returning a `List`!
  def keys[I, A: Indexable[I, ?]](fa: F[A]): List[I]

  def filter[I, A: Indexable[I, ?]](fa: F[A])(f: A => Boolean): F[A]

  def contains[I, A: Indexable[I, ?]](fa: F[A])(i: I): Boolean =
    get[I, A](fa)(i).isDefined
}

class MappableOps[F[_], A, I](val self: F[A])(implicit
    M: Mappable[F],
    I: Indexable[I, A]) {

  def +(a: A): F[A] = M.+(self)(a)

  def -(i: I): F[A] = M.-(self)(i)

  def get(i: I): Option[A] = M.get(self)(i)

  def keys: List[I] = M.keys(self)

  def filter(f: A => Boolean): F[A] = M.filter(self)(f)

  def contains(i: I): Boolean = M.contains(self)(i)
}

trait ToMappableOps {
  implicit def toMappableOps[F[_], A, I](v: F[A])(implicit
      M: Mappable[F],
      I: Indexable[I, A]) =
    new MappableOps[F, A, I](v)(M, I)
}
