package com.hablapps.geofence
package akka

import _root_.akka.NotUsed
import _root_.akka.actor.ActorSystem
import _root_.akka.stream._
import _root_.akka.stream.scaladsl._

object GeofencesAkka extends App {

  implicit val system = ActorSystem("GeoFences")
  implicit val materializer = ActorMaterializer()

  // Business Logic
  case class State(tick: Int, positions: Map[Entity,List[(Geofence,Int)]], newEvents: List[String])
  def processEvent(state: State, event: Event, fences: List[Geofence]): State = event match {
    case Tick =>
      val sideEffect = {() => println(s"TICKS: ${state.tick + 1}")}
      state.copy(tick = state.tick + 1, newEvents = s"TICKS: ${state.tick + 1}" :: Nil)
    case Halt(e: Entity) =>
      val out = state.positions.getOrElse(e, Nil)
      val events = out map {
        case (geo, tick) => s"=> ${e.name} is leaving '${geo.name}' after '${state.tick-tick}' ticks"
      }
      state.copy(positions = state.positions - e, newEvents = events) 
    case Position(e: Entity, c: Coordinate) =>
      val in = fences.filter(_.coordInFence(c)).map((_,state.tick))
      val out = state.positions.getOrElse(e, Nil) filter {
        case (geo, _) => !geo.coordInFence(c)
      }
      val inEvents = in map {
        case (geo, _) => s"=> ${e.name} is entering '${geo.name}'"
      }
      val outEvents = out map {
        case (geo, tick) => s"=> ${e.name} is leaving '${geo.name}' after '${state.tick-tick}' ticks"
      }
      state.copy(positions = state.positions + (e -> in), newEvents = inEvents ::: outEvents)
  }

  // Pure data
  val fences = List(Geofence("a", Coordinate(1,1), 2, 3),
                    Geofence("b", Coordinate(1,1), 2, 3))
  val initialState: State = State(0, Map[Entity,List[(Geofence,Int)]](), List.empty[String])
  val lines: Source[String, NotUsed] = Source(
    "tick" ::
    "tick" ::
    "coche 2 2" ::
    "tick" ::
    "tick" ::
    "coche 10 12" ::
    "tick" ::
    "tick" ::
    Nil)

  // Flows
  val events: Source[Event, NotUsed] =
    lines
      .map(Event.parse)
      .filter(_.isDefined)
      .map(_.get)

  val extractEvents: Flow[State, String, NotUsed] =
    Flow[State]
      .mapConcat(_.newEvents)

  val eventsConsumer: Sink[Event, NotUsed] = Sink.fromGraph(GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    // Components
    val zip = b.add(ZipWith((state: State, event: Event) => processEvent(state, event, fences)))
    val bcast = b.add(Broadcast[State](2))
    val concat = b.add(Concat[State]())

    // Graph
    zip.in0 <~ concat <~ Source.single(initialState)
               concat <~ bcast
    zip.out ~>           bcast ~> extractEvents ~> Sink.foreach[String](println)

    SinkShape(zip.in1)
  })

  events.runWith(eventsConsumer)

}
