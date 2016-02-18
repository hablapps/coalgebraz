package org.hablapps.geofence.state

import scala.concurrent.duration._

trait Joinable[A] {
  def join(a: A)(id: String): A
  def leave(a: A)(id: String): A
  // XXX: duration? yeap, suits `Joinable` perfectly! </ironic>
  def find(a: A)(id: String): Option[(String, Duration)]
}

class JoinableOps[A](val a: A)(implicit J: Joinable[A]) {
  def join(id: String): A = J.join(a)(id)
  def leave(id: String): A = J.leave(a)(id)
  def find(id: String): Option[(String, Duration)] = J.find(a)(id)
}

trait ToJoinableOps {
  implicit def toJoinableOps[A](a: A)(implicit P: Joinable[A]) =
    new JoinableOps[A](a)(P)
}
