package org.hablapps.geofence

import org.hablapps.coalgebraz._

// XXX: this pattern requires a lot of boilerplate! Could we use a macro to
// avoid it? ie. `@infix trait ...`
trait Identifiable[A] {
  def id(a: A): String
}

class IdentifiableOps[A](val a: A)(implicit I: Identifiable[A]) {
  def id: String = I.id(a)
}

trait ToIdentifiableOps {
  implicit def toIdentifiableOps[A](a: A)(implicit I: Identifiable[A]) =
    new IdentifiableOps[A](a)(I)
}

trait Positionable[A] {
  def position(a: A): (Int, Int)
  def position_=(a: A)(p: (Int, Int)): A
}

class PositionableOps[A](val a: A)(implicit P: Positionable[A]) {
  def position: (Int, Int) = P.position(a)
  def position_=(pos: (Int, Int)): A = P.position_=(a)(pos)
}

trait ToPositionableOps {
  implicit def toPositionableOps[A](a: A)(implicit P: Positionable[A]) =
    new PositionableOps[A](a)(P)
}

trait Tickable[A] {
  def tick(a: A): A
}

object Tickable {
  implicit val intInstance = new Tickable[Int] {
    def tick(a: Int) = a + 1
  }
}

class TickableOps[A](val a: A)(implicit T: Tickable[A]) {
  def tick: A = T.tick(a)
}

trait ToTickableOps {
  implicit def toTickableOps[A](a: A)(implicit P: Tickable[A]) =
    new TickableOps[A](a)(P)
}

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

trait Domain extends ToPositionableOps
  with ToIdentifiableOps
  with ToTickableOps
  with ToJoinableOps

case class Geolocation(id: String, pos: (Int, Int))

object Geolocation {
  implicit val positionableInstance = new Positionable[Geolocation] {
    def position(a: Geolocation) = a.pos
    def position_=(a: Geolocation)(p: (Int, Int)) = a.copy(pos = p)
  }

  implicit val identifiableInstance = new Identifiable[Geolocation] {
    def id(a: Geolocation): String = a.id
  }
}

sealed trait GeolocationIn
case class Move(pos: (Int, Int)) extends GeolocationIn
case object Halt extends GeolocationIn

sealed trait GeolocationOut
case class Moved(pos: (Int, Int)) extends GeolocationOut
case object Halted extends GeolocationOut

case class Geofence(
    id: String,
    pos: (Int, Int),
    radius: Int,
    relation: Set[(String, Int)] = Set()) {

  def elements = relation.map(_._1)

  def covers(pos2: (Int, Int)): Boolean = {
    def f(a: Int, b: Int) = a <= b + radius && a >= b - radius
    f(pos2._1, pos._1) && f(pos2._2, pos._2)
  }
}

sealed trait GeofenceIn
case class Join(id: String) extends GeofenceIn
case class Leave(id: String) extends GeofenceIn

sealed trait GeofenceOut
case class Joined(id: String) extends GeofenceOut
case class Left(id: String, after: Int) extends GeofenceOut

sealed trait ClockOut
case object Tick extends ClockOut
