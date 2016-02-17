package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Coalgebraz._, dsl._, EntityOps._

object Geomonitor extends Domain with Routing {

  val geoentity: Entity[GeolocationIn, GeolocationOut, Geolocation, Geolocation] =
    next(implicit x => {
      case Move(pos2) if (x.pos != pos2) => x.copy(pos = pos2) ~> Moved(pos2)
      case Halt => halt ~> Halted
      case _ => skip
    })

  def geoentity2[X : Positionable : Identifiable]
      : Entity[GeolocationIn, GeolocationOut, (String, (Int, Int)), X]=
    raw(x => (x.id, x.position), implicit x => {
      case Move(p) if (x.position != p) => (x.position = p) ~> Moved(p)
      case Halt => halt ~> Halted
      case _ => skip
    })

  val geofence: Entity[ClockOut \/ GeofenceIn, GeofenceOut, Geofence, Geofence] =
    next(implicit x => {
      case -\/(Tick) => x.copy(relation = x.relation.map(_.map(_ + 1)))
      case \/-(Join(id)) =>
        x.copy(relation = x.relation + (id -> 0)) ~> Joined(id)
      case \/-(Leave(id)) => {
        x.relation.find(_._1 == id).fold(skip[GeofenceOut, Geofence]) { t =>
          x.copy(relation = x.relation - t) ~> Left(id, t._2)
        }
      }
    })

  def geofence2[X : Tickable : Joinable]
      : Entity[ClockOut \/ GeofenceIn, GeofenceOut, X, X] =
    next(implicit x => {
      case -\/(Tick) => x.tick
      case \/-(Join(id)) => x.join(id) ~> Joined(id)
      case \/-(Leave(id)) => x.find(id).fold(skip[GeofenceOut, X]) { t =>
        x.leave(id) ~> Left(id, t._2.toSeconds.toInt)
      }
    })

  val timer: Entity[Unit, ClockOut, Int, Int] =
    next(implicit x => _ => (x + 1) ~> Tick)

  def timer2[X: Tickable]: Entity[Unit, ClockOut, X, X] =
    next(implicit x => _ => x.tick ~> Tick)

  // Entity[
  //   IndexIn[GeoentityIn, Geoentity, String],
  //   IndexOut[GeoentityOut, Geoentity, String],
  //   Map[String, Geoentity],
  //   List[Geoentity]]
  val geoentities = geoentity.index(_.id, identity)

  // Entity[
  //   Unit \/ IndexIn[GeoentityIn, Geoentity, String],
  //   ClockOut \/ IndexOut[GeoentityOut, Geoentity, String],
  //   (Int, Map[String, Geoentity]),
  //   (Int, List[Geoentity])]
  val timerAndGeoentities = timer |*| geoentities

  // Entity[
  //   IndexIn[GeofenceIn, Geofence, String],
  //   IndexOut[GeofenceOut, Geofence, String],
  //   Map[String, Geofence],
  //   List[Geofence]]
  val geofences = geofence.index(_.id, identity)

  // FIXME: It's required to invoke `in` to force `geofences` to adapt its
  // input. Can this be avoided?
  //
  // Entity[
  //   IndexIn[GeoentityIn, Geoentity, String],
  //   IndexOut[GeofenceOut, Geofence, String],
  //   (Map[String, Geoentity], Map[String, Geofence]),
  //   (List[Geoentity], List[Geofence])]
  val monitor = timerAndGeoentities |>| geofences.in
}
