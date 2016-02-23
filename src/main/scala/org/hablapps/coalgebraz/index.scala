package org.hablapps.coalgebraz

sealed trait IndexIn[+I, +B, +N]
case class Attach[B, N](nv: (N, B)) extends IndexIn[Nothing, B, N]
case class Detach[N](n: N) extends IndexIn[Nothing, Nothing, N]
case class WrapIn[I, N](i: (N, I)) extends IndexIn[I, Nothing, N]

sealed trait IndexOut[+O, +B, +N]
case class Attached[B, N](nv: (N, B)) extends IndexOut[Nothing, B, N]
case class Detached[N](n: N) extends IndexOut[Nothing, Nothing, N]
case class WrapOut[O, N](os: (N, O)) extends IndexOut[O, Nothing, N]
case class UnknownIndex[N](n: N) extends IndexOut[Nothing, Nothing, N]

object wrap {
  object dsl {
    implicit class WrapHelper[N, I](i: (N, I)) {
      def wrap: WrapIn[I, N] = WrapIn(i)
    }
  }
}
