package coalgebraz

class StreamOps[H, X](val self: Stream[H, X]) {


}

trait ToStreamOps {
  implicit def toStreamOps[H, X](s: Stream[H, X]): StreamOps[H, X] =
    new StreamOps(s)
}
