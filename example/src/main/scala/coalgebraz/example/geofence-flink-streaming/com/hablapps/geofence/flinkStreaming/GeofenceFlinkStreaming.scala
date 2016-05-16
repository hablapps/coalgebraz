package com.hablapps.geofence.flinkStreaming

import org.apache.flink.streaming.api.scala._
import org.apache.flink.streaming.api.windowing.assigners.TumblingEventTimeWindows
import org.apache.flink.streaming.api.windowing.time.Time

import com.hablapps.geofence._ // Domain

object FlinkStreamingGeofence extends App {

  val env = StreamExecutionEnvironment.getExecutionEnvironment
  env.getConfig.disableSysoutLogging

  val lines: DataStream[String] = env.socketTextStream("localhost",9999)
  val events: DataStream[Event] = lines.map(Event.parse _).filter(_.isDefined).map(_.get)

  val fences = List(Geofence("a", Coordinate(1,1), 2, 3),
                    Geofence("b", Coordinate(1,1), 2, 3))

  val eventsPaired: KeyedStream[Event,Unit] = events.keyBy((e:Event) => ())

  import StatelessWatcherSideEffects.State
  val initialState: State = State(0, Map[Entity,List[(Geofence,Int)]](), List[() => Unit]())

  def updateState(event: Event, stateO: Option[State]): (List[() => Unit], Option[State]) = {
    val presentState: State = stateO.getOrElse(initialState).copy(sideEffects = List[() => Unit]())
    val newState: State = StatelessWatcherSideEffects.processEvent(presentState, event, fences)
    (newState.sideEffects, Some(newState))
  }

  val sideEffects: DataStream[List[() => Unit]] = eventsPaired.mapWithState(updateState _)

  sideEffects.addSink(_.foreach(sideEffect => sideEffect())) // Processing side effects
                    
  env.execute("FlinkStreamingGeofence")

}
