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
