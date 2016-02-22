package org.hablapps.coalgebraz

import Coalgebraz._

trait Mappable[F[_]] {

  def +[I, A: Indexable[I, ?]](fa: F[A])(a: A): F[A]

  def -[I, A: Indexable[I, ?]](fa: F[A])(i: I): F[A]

  def get[I, A: Indexable[I, ?]](fa: F[A])(i: I): Option[A]

  // XXX: weak link, we are returning a `List`!
  def keys[I, A: Indexable[I, ?]](fa: F[A]): List[I]

  def filter[I, A: Indexable[I, ?]](fa: F[A])(f: (I, A) => Boolean): F[A]

  def contains[I, A: Indexable[I, ?]](fa: F[A])(i: I): Boolean =
    get[I, A](fa)(i).isDefined
}

object Mappable {

  implicit def mappableList = new Mappable[List] {

    def +[I, A: Indexable[I, ?]](fa: List[A])(a: A): List[A] =
      a :: (this.-(fa)(a.index))

    def -[I, A: Indexable[I, ?]](fa: List[A])(i: I): List[A] =
      fa.filter(_.index != i)

    def get[I, A: Indexable[I, ?]](fa: List[A])(i: I): Option[A] =
      fa.find(_.index == i)

    def keys[I, A: Indexable[I, ?]](fa: List[A]): List[I] =
      fa.map(_.index)

    def filter[I, A: Indexable[I, ?]](fa: List[A])(f: (I, A) => Boolean): List[A] =
      fa.filter(a => f(a.index, a))
  }
}

class MappableOps[F[_], A, I](val self: F[A])(implicit
    M: Mappable[F],
    I: Indexable[I, A]) {

  def +(a: A): F[A] = M.+(self)(a)

  def -(i: I): F[A] = M.-(self)(i)

  def get(i: I): Option[A] = M.get(self)(i)

  def keys: List[I] = M.keys(self)

  def filter(f: (I, A) => Boolean): F[A] = M.filter(self)(f)

  def contains(i: I): Boolean = M.contains(self)(i)
}

trait ToMappableOps {
  implicit def toMappableOps[F[_], A, I](v: F[A])(implicit
      M: Mappable[F],
      I: Indexable[I, A]) =
    new MappableOps[F, A, I](v)(M, I)
}
