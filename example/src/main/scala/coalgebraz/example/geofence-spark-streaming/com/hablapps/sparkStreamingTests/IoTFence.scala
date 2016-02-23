package com.hablapps.sparkStreamingTests

import org.apache.spark._
import org.apache.spark.streaming._

import com.hablapps.sparkStreamingTests.geofence._

object IoTFence extends App {
  
  val watcher = Watcher(List(Geofence("a", Coordinate(1,1), 2, 3),
                             Geofence("b", Coordinate(1,1), 2, 3)) )
  
  val conf = new SparkConf().setAppName("IoTFence").setMaster("local[2]")
  
  val batchInterval = Seconds(5) 
  val ssc = new StreamingContext(conf, batchInterval)
  
  val lines = ssc.socketTextStream("localhost", 9999)
  
  val events = lines.map(Event.parse).filter(_.isDefined).map(_.get) // parse
  
  events.foreachRDD(rdd => rdd.foreach(watcher.event(_))) // process
  
  ssc.start() 
  ssc.awaitTermination() 
}