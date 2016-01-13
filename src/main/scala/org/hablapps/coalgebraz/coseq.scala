package org.hablapps.coalgebraz

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._, Scalaz._

trait Sq[F[_], G[_]] {
  def apply[A](fga: F[G[A]]): G[F[A]]
}

object Sq {

  implicit def allOrNone: Sq[List, Option] = new Sq[List, Option] {
    def apply[A](fga: List[Option[A]]): Option[List[A]] = fga.sequence
  }

  implicit def someOrNone: Sq[List, Option] = new Sq[List, Option] {
    def apply[A](fga: List[Option[A]]): Option[List[A]] =
      fga.flatMap(_.toList) match {
        case Nil => None
        case xs  => Some(xs)
      }
  }
}

sealed trait CoseqIn[I, B, X]
case class Elem[I, B, X](f: B => Option[I]) extends CoseqIn[I, B, X]
case class Prepend[I, B, X](value: X) extends CoseqIn[I, B, X]

sealed trait CoseqOut[O, X]
case class WrappedOut[O, X](os: NonEmptyList[O]) extends CoseqOut[O, X]
case class Prepended[O, X](value: X) extends CoseqOut[O, X]
case class Removed[O, X](value: X) extends CoseqOut[O, X]
