package com.hablapps.geofence.sparkStreaming

import org.apache.spark._
import org.apache.spark.streaming._
import org.apache.spark.streaming.dstream.DStream

import com.hablapps.geofence._ // Domain

object SparkStreamingGeofence extends App {
  
  val conf = new SparkConf().setAppName("IoTFence").setMaster("local[2]")
  
  val batchInterval = Seconds(5) 
  val ssc = new StreamingContext(conf, batchInterval)
  
  val lines: DStream[String] = ssc.socketTextStream("localhost", 9999)
  val events: DStream[Event] = lines.map(Event.parse).filter(_.isDefined).map(_.get) // parse
  
  val fences = List(Geofence("a", Coordinate(1,1), 2, 3),
                    Geofence("b", Coordinate(1,1), 2, 3))
                    
  /*
  // -- STATE KEPT BY WATCHER --
  val watcher = Watcher(fences)
  
  events.foreachRDD(rdd => rdd.foreach(watcher.event(_))) // process
  // -- STATE KEPT BY WATCHER --
  */
  
  
  /*
  // -- STATE KEPT BY SPARK --
  val eventsPaired = events.map(((),_)) // Spark keeps state by key. In our
                                        // case state is global (i.e. unique)
                                        // so there is only one key. We use
                                        // Unit type and its only instance '()'
  
  import StatelessWatcher.State
  val initialState = State(0, Map[Entity,List[(Geofence,Int)]]())
  
  def updateState(events: Seq[Event], stateO: Option[State]): Option[State] = {
    val presentState = stateO.getOrElse(initialState)    
    val newState = events.foldLeft[State](presentState)((s:State, e: Event) => StatelessWatcher.processEvent(s, e, fences))
    Some(newState)
  }
  
  eventsPaired.updateStateByKey[State](updateState _)
              .foreachRDD(_.map(_._2).foreach(_ => ())) // To force processing
  ssc.checkpoint("./checkpoints")  // Mandatory when keeping state, so Spark knows where to save checkpoint it
  // -- STATE KEPT BY SPARK -- 
  */
                    
                    
  // -- STATE KEPT BY SPARK, TRANSFORMATION TO SIDE EFFECTS DSTREAM --
  val eventsPaired: DStream[(Unit,Event)] = events.map(((),_)) // Spark keeps state by key. In our
                                                              // case state is global (i.e. unique)
                                                              // so there is only one key. We use
                                                              // Unit type and its only instance '()'
  
  import StatelessWatcherSideEffects.State
  val initialState: State = State(0, Map[Entity,List[(Geofence,Int)]](), List[() => Unit]())
  
  def updateState(events: Seq[Event], stateO: Option[State]): Option[State] = {
    val presentState: State = stateO.getOrElse(initialState).copy(sideEffects = List[() => Unit]())
    val newState: State = events.foldLeft[State](presentState)((s:State, e: Event) => StatelessWatcherSideEffects.processEvent(s, e, fences))
    Some(newState)
  }
  
  val sideEffects: DStream[List[() => Unit]] = eventsPaired.updateStateByKey[State](updateState _).map(_._2.sideEffects.toList)  
  sideEffects.foreachRDD(_.foreach(l => l.foreach(sideEffect => sideEffect()))) // To force processing
  ssc.checkpoint("./checkpoints")  // Mandatory when keeping state, so Spark knows where to save checkpoint it
  // -- STATE KEPT BY SPARK, TRANSFORMATION TO SIDE EFFECTS DSTREAM --
                    
  ssc.start() 
  ssc.awaitTermination() 
}
