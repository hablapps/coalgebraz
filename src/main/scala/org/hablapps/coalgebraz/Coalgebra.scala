package org.hablapps.coalgebraz

import org.hablapps.coalgebraz

object Coalgebra {

  def always[A](value: A): Coentity[Unit, Nothing, A, A] =
    _ => Entity(value, _ => (List.empty, Option(value)))

  def constant[A]: Coentity[Unit, Nothing, A, A] =
    s => Entity(s, _ => (List.empty, Option(s)))
}
