package coalgebraz.example.geofence.state

trait Joinable[A] {

  def join(a: A)(id: String): A

  def leave(a: A)(id: String): A

  // XXX: duration (Long)? yeap, suits `Joinable` perfectly! </ironic>
  def find(a: A)(id: String): Option[(String, Long)]

  def contains(a: A)(id: String): Boolean = find(a)(id).isDefined
}

class JoinableOps[A](val self: A)(implicit J: Joinable[A]) {

  def join(id: String): A = J.join(self)(id)

  def leave(id: String): A = J.leave(self)(id)

  def find(id: String): Option[(String, Long)] = J.find(self)(id)

  def contains(id: String): Boolean = J.contains(self)(id)
}

trait ToJoinableOps {
  implicit def toJoinableOps[A](a: A)(implicit P: Joinable[A]) =
    new JoinableOps[A](a)(P)
}
