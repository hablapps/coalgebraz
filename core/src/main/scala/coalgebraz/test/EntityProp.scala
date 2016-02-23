package coalgebraz.test

import Function.const

import coalgebraz._

sealed trait EntityProp[A]
case class Pred[A](f: A => Boolean) extends EntityProp[A]
case class Next[A](f: A => EntityProp[A]) extends EntityProp[A]
case class Not[A](f: A => EntityProp[A]) extends EntityProp[A]
case class And[A](
  f1: A => EntityProp[A],
  f2: A => EntityProp[A]) extends EntityProp[A]

object EntityProp {

  def pred[A](f: A => Boolean): EntityProp[A] = Pred(f)

  def next[A](f: A => EntityProp[A]): EntityProp[A] = Next(f)

  def not[A](f: A => EntityProp[A]): EntityProp[A] = Not(f)

  def just[A](b: Boolean): EntityProp[A] = pred(const(b))

  def always[A](f: A => EntityProp[A]): EntityProp[A] =
    and(f, _ => next(_ => always(f)))

  def exists[A](f: A => EntityProp[A]): EntityProp[A] =
    or(f, _ => next(_ => exists(f)))

  def and[A](
      p1: A => EntityProp[A],
      p2: A => EntityProp[A]): EntityProp[A] =
    And(p1, p2)

  def or[A](
      f1: A => EntityProp[A],
      f2: A => EntityProp[A]): EntityProp[A] =
    not(_ => and(_ => not(f1), _ => not(f2)))
}
