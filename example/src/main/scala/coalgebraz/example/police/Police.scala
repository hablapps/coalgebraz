package com.hablapps.police
package akka

import _root_.akka.NotUsed
import _root_.akka.actor.ActorSystem
import _root_.akka.stream._
import _root_.akka.stream.scaladsl._

object Police extends App {

  implicit val system = ActorSystem("Police")
  implicit val materializer = ActorMaterializer()

  // Pure data
  val initialState: State = State(
    tick = 0,
    fences = List.empty[Geofence],
    entities = List.empty[Position],
    belongings = List.empty[Belonging])
  val lines: Source[String, NotUsed] = Source(
    "tick" ::
    "geofenceup fence1 10 10 10" ::
    "tick" ::
    "geofenceup fence2 30 20 20" ::
    "tick" ::
    "position coche1 5 5" ::
    "tick" ::
    "position coche2 40 20" ::
    "tick" ::
    "position coche3 25 10" ::
    "tick" ::
    "geofenceup fence3 20 10 10" ::
    "tick" ::
    "position coche3 10 30" ::
    "tick" ::
    "geofencedown fence3" ::
    "tick" ::
    Nil)

  // Business Logic
  def processEvent(state: State, event: InputEvent): (State, List[String]) = event match {
    case Tick =>
      (state.copy(tick = state.tick + 1), s"TICKS: ${state.tick + 1}" :: Nil)
    case Halt(eId: String) =>
      val position = state.entities.find(_.e.name == eId).get
      val out = state.belongings.collect {
        case Belonging(geo, Entity(`eId`), tick) => (geo, tick)
      }
      val events = out map {
        case (geo, tick) => s"=> ${position.e.name} is leaving '${geo.name}' after '${state.tick-tick}' ticks"
      }
      (state.copy(
        entities = state.entities.filter(_.e.name == eId),
        belongings = state.belongings.filter(_.entity.name == eId)), events)
    case p@Position(e: Entity, c: Coordinate) =>
      val in = state.fences.filter(_.coordInFence(c)).map(Belonging(_, e,state.tick))
      val out = state.belongings.filter(_.entity.name == e.name)
      val inEvents = in map {
        case Belonging(geo, _, _) => s"=> ${e.name} is entering '${geo.name}'"
      }
      val outEvents = out map {
        case Belonging(geo, _, tick) => s"=> ${e.name} is leaving '${geo.name}' after '${state.tick-tick}' ticks"
      }
      (state.copy(
        entities = state.entities.find(_.e == e).fold(p :: state.entities)(_ => state.entities),
        belongings = (state.belongings diff out) ::: in), inEvents ::: outEvents)
    case GeofenceUp(name, center, radius) =>
      val geo = Geofence(name, center, radius)
      val newBelongings = state.entities filter { p =>
        geo.coordInFence(p.c)
      } map { p =>
        Belonging(geo, p.e, state.tick)
      }
      (state.copy(
        fences = geo :: state.fences,
        belongings = newBelongings ::: state.belongings),
        s"New fence created $name with coords ${center.x} ${center.y} and radius ${radius}" ::
        newBelongings.map(b => s"=> ${b.entity.name} is entering '${b.geo.name}'"))
    case GeofenceDown(name) =>
      val belongingsRelated = state.belongings.filter { b =>
        b.geo.name == name
      }
      (state.copy(
        fences = state.fences.filter(_.name != name),
        belongings = state.belongings diff belongingsRelated),
        belongingsRelated.map(b => s"=> ${b.entity.name} is leaving '${b.geo.name}' after '${state.tick-b.tick}' ticks"))
  }

  // Flows

  val processRules: Flow[State, List[OutputEvent], NotUsed] =
    Flow[State] map { s =>
      Rules.all flatMap (_(s))
    }

  val eventParser: Flow[String, InputEvent, NotUsed] =
    Flow[String]
      .map(InputEvent.parse)
      .collect {
        case Some(e) => e
      }

  val eventsConsumer: Sink[InputEvent, NotUsed] = Sink.fromGraph(GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    // Components
    val zip = b.add(ZipWith((state: State, event: InputEvent) => processEvent(state, event)))
    val zip2 = b.add(ZipWith((events: List[String], alarms: List[OutputEvent]) => events ::: alarms.map(_.toString)))
    val unzip = b.add(Unzip[State, List[String]]())
    val bcast = b.add(Broadcast[State](2))
    val concat = b.add(Concat[State]())

    // Graph
    Source.single(initialState) ~> concat ~> zip.in0
    zip.out ~> unzip.in
               unzip.out0 ~> bcast ~> concat
               unzip.out1 ~> zip2.in0
               bcast ~> processRules ~> zip2.in1
               zip2.out ~> Flow[List[String]].mapConcat(identity) ~> Sink.foreach[String](println)

    SinkShape(zip.in1)
  })

  lines via eventParser runWith eventsConsumer

}
