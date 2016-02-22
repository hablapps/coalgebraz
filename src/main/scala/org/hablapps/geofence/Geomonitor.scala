package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Coalgebraz._, dsl._

import state._

object Geomonitor extends state.State with Routing {

  def geoentity[B, X : Observable[B, ?] : Positionable]
      : Entity[GeolocationIn, GeolocationOut, B, X] =
    next2(implicit x => {
      case Move(p) if (x.position != p) => (x.position = p) ~> Moved(p)
      case Halt => halt ~> Halted
      case _ => skip
    })

  def geofence[B, X : Observable[B, ?] : Tickable : Joinable]
      : Entity[ClockOut \/ GeofenceIn, GeofenceOut, B, X] =
    next2(implicit x => {
      case -\/(Tick) => x.tick
      case \/-(Join(id)) => x.join(id) ~> Joined(id)
      case \/-(Leave(id)) => x.find(id).fold(skip[GeofenceOut, X]) { t =>
        x.leave(id) ~> Left(id, t._2)
      }
    })

  def timer[B, X : Observable[B, ?] : Tickable]: Entity[Unit, ClockOut, B, X] =
    next2(implicit x => _ => x.tick ~> Tick)

  def geoentities[
    F[_, _] : Mappable,
    B : To[?, X],
    X : Observable[B, ?] : Positionable,
    N](implicit ev0: Functor[F[N, ?]])
      : IndexedEntity2[GeolocationIn, GeolocationOut, F, B, X, N] =
    geoentity.index2[F, N]

  def timerAndGeoentities[
      B1,
      X1 : Observable[B1, ?] : Tickable,
      F[_, _] : Mappable,
      B2: To[?, X2],
      X2 : Observable[B2, ?] : Positionable,
      N2](implicit ev0: Functor[F[N2, ?]]) =
    timer[B1, X1] |*| geoentities[F, B2, X2, N2]

  def geofences[
      F[_, _] : Mappable,
      B: To[?, X],
      X : Observable[B, ?] : Tickable : Joinable,
      N](implicit ev0: Functor[F[N, ?]]) =
    geofence.index2[F, N]

  def monitor[
      B1,
      X1 : Observable[B1, ?] : Tickable,
      F[_, _] : Mappable,
      B2: To[?, X2],
      X2 : Observable[B2, ?] : Positionable,
      N2,
      B3 : To[?, X3] : Joinable : Coverable,
      X3 : Observable[B3, ?] : Tickable : Joinable,
      N3](implicit ev0: Functor[F[N2, ?]], ev1: Functor[F[N3, ?]]) =
    timerAndGeoentities[B1, X1, F, B2, X2, N2] |>| geofences[F, B3, X3, N3].in
}
