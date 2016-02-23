package com.hablapps.sparkStreamingTests.geofence

import scala.util.Try

case class Coordinate(x: Int, y: Int)
case class Geofence(name: String, corner: Coordinate, length: Int, height: Int) {
  def coordInFence(c: Coordinate): Boolean =
    ((corner.x <= c.x) && (c.x <= corner.x + length) &&
     (corner.y <= c.y) && (c.y <= corner.y + height))
}
case class Entity(name:String)

sealed abstract class Event
object Event {
  def parse(s:String): Option[Event] = s.trim.split("\\s+") match {
    case arr if((arr.size == 1) && (arr(0).equalsIgnoreCase("tick"))) => Some(Tick)
    case arr if((arr.size == 2) && (arr(0).equalsIgnoreCase("halt"))) => Some(Halt(Entity(arr(1))))
    case arr if(arr.size == 3) => for {
        e <- Some(Entity(arr(0)))
        x <- Try(arr(1).toInt).toOption
        y <- Try(arr(2).toInt).toOption
      } yield Position(e, Coordinate(x, y))
    case _ => None
  }
}
case class Position(e: Entity, c: Coordinate) extends Event
case class Halt(e: Entity) extends Event
case object Tick extends Event

case class Watcher(val fences: List[Geofence]) {
  // For each entity, fence where it is and when it entered
  val positions = scala.collection.mutable.Map[Entity,List[(Geofence,Int)]]() 
  var tick = 0
  def event(e: Event): Unit = e match {
    case Tick => {
      tick += 1
      println(s"TICKS: $tick")
    }
    case Halt(e: Entity) => {
      val out:List[(Geofence,Int)] = positions.remove(e).getOrElse(Nil)
      out.foreach{case (f:Geofence,t:Int) => println(s"=> ${e.name} is leaving '${f.name}' after '${tick-t}' ticks")}
    }
    case Position(e:Entity, c: Coordinate) => {
      val in:List[(Geofence,Int)] = fences.filter(_.coordInFence(c)).map((_,tick))
      val out:List[(Geofence,Int)] = positions.getOrElse(e,Nil).filter{case (f:Geofence,t:Int) => !f.coordInFence(c)}
      in.foreach{case (f:Geofence,t:Int) => println(s"=> ${e.name} is entering '${f.name}'")}
      out.foreach{case (f:Geofence,t:Int) => println(s"=> ${e.name} is leaving '${f.name}' after '${tick-t}' ticks")}
      positions.remove(e)
      positions.put(e, in)
    }    
  }
}
