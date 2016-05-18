package com.hablapps.police
package akka

import scala.util.Try

// DOMAIN
case class Coordinate(x: Int, y: Int)
case class Geofence(name: String, center: Coordinate, radius: Int) {
  def coordInFence(c: Coordinate): Boolean = {
    val cat1 = math.pow(center.x - c.x, 2)
    val cat2 = math.pow(center.y - c.y, 2)
    math.sqrt(cat1 + cat2) < radius
  }
}
case class Entity(name: String)

// INPUT EVENTS
sealed abstract class InputEvent
object InputEvent {
  def parse(s: String): Option[InputEvent] = s.trim.split(" ").toList match {
    case "position" :: e :: x :: y :: Nil =>
      (for {
        x <- Try(x.toInt)
        y <- Try(y.toInt)
      } yield Position(Entity(e), Coordinate(x, y))).toOption
    case "geofenceup" :: n :: x :: y :: r :: Nil =>
      (for {
        x <- Try(x.toInt)
        y <- Try(y.toInt)
        r <- Try(r.toInt)
      } yield GeofenceUp(n, Coordinate(x, y), r)).toOption
    case "geofencedown" :: e :: Nil => Option(GeofenceDown(e))
    case "halt" :: e :: Nil => Option(Halt(e))
    case "tick" :: Nil => Option(Tick)
    case _ => Option.empty
  }
}
case class GeofenceUp(name: String, center: Coordinate, radius: Int) extends InputEvent
case class GeofenceDown(name: String) extends InputEvent
case class Position(e: Entity, c: Coordinate) extends InputEvent
case class Halt(eId: String) extends InputEvent
case object Tick extends InputEvent

// OUTPUT EVENTS
trait OutputEvent
case class TooManyEntitiesInFence(fence: Geofence, entities: List[Entity]) extends OutputEvent
case class EmptyFence(fence: Geofence) extends OutputEvent

// STATE
case class Belonging(geo: Geofence, entity: Entity, tick: Int)
case class State(tick: Int, fences: List[Geofence], entities: List[Position], belongings: List[Belonging])
