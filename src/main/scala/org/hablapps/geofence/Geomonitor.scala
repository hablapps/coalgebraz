package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Coalgebraz._, dsl._, EntityOps._

import Routing._

object Geomonitor {

  val geoentity: Entity[GeolocationIn, GeolocationOut, Geolocation, Geolocation] =
    next(implicit x => {
      case Move(pos2) if (x.pos != pos2) => x.copy(pos = pos2) ~> Moved(pos2)
      case Halt => halt ~> Halted
      case _ => skip
    })

  /**************************/

  // trait Positionable
  //
  // trait Geoentitizable[A] {
  //   def getPos(a: A): (Int, Int)
  //   def setPos(a: A)(pos: (Int, Int)): A
  //   def obs(a: A): Geoentity
  // }
  //
  // object Geoentitizable {
  //
  //   def apply[A](implicit ev: Geoentitizable[A]): Geoentitizable[A] = ev
  //
  //   implicit val geoentitizableGeoentity = new Geoentitizable[Geoentity] {
  //     def getPos(a: Geoentity) = a.pos
  //     def setPos(a: Geoentity)(_pos: (Int, Int)) = a.copy(pos = _pos)
  //     def obs(a: Geoentity) = a
  //   }
  // }
  //
  // class GeoentitizableOps[A](val a: A)(implicit P: Geoentitizable[A]) {
  //   def getPos: (Int, Int) = P.getPos(a)
  //   def setPos(pos: (Int, Int)): A = P.setPos(a)(pos)
  //   def obs: Geoentity  = P.obs(a)
  // }
  //
  // object GeoentitizableOps {
  //   implicit def toGeoentitizableOps[A: Geoentitizable](a: A): GeoentitizableOps[A] =
  //     new GeoentitizableOps(a)
  // }
  //
  // import GeoentitizableOps._
  //
  // def geoentity2[X: Geoentitizable]: Entity[GeoentityIn, GeoentityOut, Geoentity, X] =
  //   implicit x => {
  //     EntityF(x.obs, {
  //       case Move(pos2) if (x.getPos != pos2) => x.setPos(pos2) ~> Moved(pos2)
  //       case Halt => halt ~> Halted
  //       case _ => skip
  //     })
  //   }

  /**************************/

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

  val clock: Entity[Unit, ClockOut, Int, Int] =
    next(implicit x => _ => (x + 1) ~> Tick)

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
  val clockAndGeoentities = clock |*| geoentities

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
  val monitor = clockAndGeoentities |>| geofences.in
}
