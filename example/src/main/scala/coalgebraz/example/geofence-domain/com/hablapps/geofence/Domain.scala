package com.hablapps.geofence

import scala.util.Try

case class Coordinate(x: Int, y: Int) {
  def distance(c: Coordinate): Double = math.sqrt( math.pow(x - c.x, 2) + math.pow(y - c.y, 2) )
}
sealed abstract class Geofence(val name: String) extends Serializable {
  def coordInFence(c: Coordinate): Boolean
}
object Geofence {
  def apply(name:String, corner: Coordinate, length: Int, height: Int): Geofence = RectGeofence(name, corner, length, height)
  def apply(name:String, center: Coordinate, radius: Int): Geofence = CircGeoFence(name, center, radius)
  def apply(name:String, subfences: List[Geofence]): Geofence = ComposedGeoFence(name, subfences)
}
case class RectGeofence(override val name: String, corner: Coordinate, length: Int, height: Int) extends Geofence(name) {
  override def coordInFence(c: Coordinate): Boolean =
    ((corner.x <= c.x) && (c.x <= corner.x + length) &&
     (corner.y <= c.y) && (c.y <= corner.y + height))
}
case class CircGeoFence(override val name: String, center: Coordinate, radius: Int) extends Geofence(name) {
  override def coordInFence(c: Coordinate): Boolean = 
    (center distance c) <= radius
}
case class ComposedGeoFence(override val name: String, subfences: List[Geofence]) extends Geofence(name) {
  override def coordInFence(c: Coordinate): Boolean = 
    subfences.find(_.coordInFence(c)).isDefined
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
      positions.put(e, in)
    }    
  }
}


case object StatelessWatcher {
  
  // State is defined by present tick and map of entities <-> geofences,
  // and it is passed in each call to process a new event.
  case class State(tick: Int, positions: Map[Entity,List[(Geofence,Int)]])
  
  def processEvent(state: State, event: Event, fences: List[Geofence]): State = event match {
    case Tick => {
      println(s"TICKS: ${state.tick + 1}")
      state.copy(tick = state.tick + 1)
    }
    case Halt(e: Entity) => {
      val out:List[(Geofence,Int)] = state.positions.getOrElse(e, Nil)
      out.foreach{case (f:Geofence,t:Int) => println(s"=> ${e.name} is leaving '${f.name}' after '${state.tick-t}' ticks")}
      state.copy(positions = state.positions - e)
    }
    case Position(e: Entity, c: Coordinate) => {
      val in:List[(Geofence,Int)] = fences.filter(_.coordInFence(c)).map((_,state.tick))
      val out:List[(Geofence,Int)] = state.positions.getOrElse(e, Nil).filter{case (f:Geofence,t:Int) => !f.coordInFence(c)}
      in.foreach{case (f:Geofence,t:Int) => println(s"=> ${e.name} is entering '${f.name}'")}
      out.foreach{case (f:Geofence,t:Int) => println(s"=> ${e.name} is leaving '${f.name}' after '${state.tick-t}' ticks")}
      state.copy(positions = state.positions + (e -> in))
    }
  }

}


case object StatelessWatcherSideEffects {
  
  // State is defined by present tick and map of entities <-> geofences,
  // and it is passed in each call to process a new event.
  case class State(tick: Int, positions: Map[Entity,List[(Geofence,Int)]], sideEffects: List[() => Unit])
  
  def processEvent(state: State, event: Event, fences: List[Geofence]): State = event match {
    case Tick => {
      val sideEffect = {() => println(s"TICKS: ${state.tick + 1}")}
      state.copy(tick = state.tick + 1, sideEffects = state.sideEffects :+ sideEffect)
    }
    case Halt(e: Entity) => {
      val out:List[(Geofence,Int)] = state.positions.getOrElse(e, Nil)
      val newSideEffects = out.map{case (f:Geofence,t:Int) => {() => println(s"=> ${e.name} is leaving '${f.name}' after '${state.tick-t}' ticks")}}
      state.copy(positions = state.positions - e, sideEffects = state.sideEffects ::: newSideEffects) 
    }
    case Position(e: Entity, c: Coordinate) => {
      val in:List[Geofence] = fences.filter(_.coordInFence(c))
      val alreadyIn: List[(Geofence,Int)] = state.positions.getOrElse(e,Nil).filter{case (f:Geofence,t:Int) => f.coordInFence(c)}
      val newlyIn: List[(Geofence,Int)] = (in diff alreadyIn.map(_._1)).map(f => (f,state.tick))
      val justOut:List[(Geofence,Int)] = state.positions.getOrElse(e,Nil).filter{case (f:Geofence,t:Int) => !f.coordInFence(c)}
      val newPositions = state.positions + (e -> (alreadyIn ::: newlyIn))
      val nonemptyGeofences:List[Geofence] = newPositions.values.toList.flatMap(l => l).map(_._1)
      val emptiedGeofences:List[Geofence] = justOut.map(_._1) diff nonemptyGeofences
      val newSideEffects = newlyIn.map{case (f:Geofence,t:Int) => {() => println(s"=> ${e.name} is entering '${f.name}'")}} :::
                           justOut.map{case (f:Geofence,t:Int) => {() => println(s"=> ${e.name} is leaving '${f.name}' after '${state.tick-t}' ticks")}} :::
                           emptiedGeofences.map{case (f:Geofence) => {() => println(s"=> ${f.name} is empty")}}
      state.copy(positions = newPositions, sideEffects = state.sideEffects ::: newSideEffects)
    }
  }

}
