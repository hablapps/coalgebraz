package com.hablapps.geofence.rxScala

import rx.lang.scala.Observable

import com.hablapps.geofence._ // Domain

object RxScalaGeofence extends App {

  val entries = "tick" :: "coche 2 2" :: "tick" :: "coche 2 20" :: Nil

  val lines: Observable[String] = Observable.from(entries)
  var events: Observable[Event] = lines.map(Event.parse).filter(_.isDefined).map(_.get)

  val fences = List(Geofence("a", Coordinate(1,1), 2, 3),
                    Geofence("b", Coordinate(1,1), 2, 3))

  import StatelessWatcherSideEffects.State
  val initialState: (State,List[() => Unit]) = (State(0, Map[Entity,List[(Geofence,Int)]](), Nil), Nil)

  val states: Observable[(State,List[() => Unit])] = events.scan(initialState){ case ((s:State,effects:List[() => Unit]), e:Event) => 
    val s2 = StatelessWatcherSideEffects.processEvent(s, e, fences)
    (s2.copy(sideEffects=Nil), s2.sideEffects)
  }

  val sideEffects: Observable[List[() => Unit]] = states.map(_._2)

  sideEffects.subscribe{(sideEffects: List[() => Unit]) => 
    sideEffects.foreach(_())
  }

}
