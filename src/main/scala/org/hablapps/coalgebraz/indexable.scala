package org.hablapps.coalgebraz

trait Indexable[I, A] {
  def index(a: A): I
}

class IndexableOps[I, A](val a: A)(implicit In: Indexable[I, A]) {
  def index: I = In.index(a)
}

trait ToIndexableOps {
  implicit def toIndexableOps[I, A](a: A)(implicit In: Indexable[I, A]) =
    new IndexableOps[I, A](a)(In)
}
