package org.hablapps.coalgebraz

sealed trait IndexIn[I, X, N]
case class Attach[I, X, N](v: X) extends IndexIn[I, X, N]
case class Detach[I, X, N](n: N) extends IndexIn[I, X, N]
case class WrapIn[I, X, N](i: (N, I)) extends IndexIn[I, X, N]

sealed trait IndexOut[O, X, N]
case class Attached[O, X, N](v: X) extends IndexOut[O, X, N]
case class Detached[O, X, N](n: N) extends IndexOut[O, X, N]
case class WrapOut[O, X, N](os: (N, O)) extends IndexOut[O, X, N]
case class UnknownIndex[O, X, N](n: N) extends IndexOut[O, X, N]
