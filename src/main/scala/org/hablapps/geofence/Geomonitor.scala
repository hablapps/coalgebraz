package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._
import Coalgebraz._, dsl._

import state._

object Geomonitor extends state.State with Routing {

  def geoentity[B, X : Observable[B, ?] : Positionable]
      : Entity[GeolocationIn, GeolocationOut, B, X] =
    entity(_.observe, implicit x => {
      case Move(p) if (x.position != p) => (x.position = p) ~> Moved(p)
      case Halt => halt ~> Halted
      case _ => skip
    })

  def geofence[B, X : Observable[B, ?] : Tickable : Joinable]
      : Entity[ClockOut \/ GeofenceIn, GeofenceOut, B, X] =
    entity(_.observe, implicit x => {
      case -\/(Tick) => x.tick
      case \/-(Join(id)) => x.join(id) ~> Joined(id)
      case \/-(Leave(id)) => x.find(id).fold(skip[GeofenceOut, X]) { t =>
        x.leave(id) ~> Left(id, t._2)
      }
    })

  def timer[B, X : Observable[B, ?] : Tickable]: Entity[Unit, ClockOut, B, X] =
    entity(_.observe, implicit x => _ => x.tick ~> Tick)

  def geoentities[
      F[_] : Functor : Mappable,
      B,
      X : Observable[B, ?] : Indexable[N, ?] : Positionable,
      N]: IndexedEntity2[GeolocationIn, GeolocationOut, F, B, X, N] =
    geoentity.index2[F, N]

  def timerAndGeoentities[
      B1,
      X1 : Observable[B1, ?] : Tickable,
      F2[_] : Functor : Mappable,
      B2,
      X2 : Observable[B2, ?] : Indexable[N2, ?] : Positionable,
      N2] =
    timer[B1, X1] |*| geoentities[F2, B2, X2, N2]

  def geofences[
      F[_] : Functor : Mappable,
      B,
      X : Observable[B, ?] : Indexable[N, ?] : Tickable : Joinable,
      N] =
    geofence.index2[F, N]

  def monitor[
      B1,
      X1 : Observable[B1, ?] : Tickable,
      F[_] : Functor : Mappable,
      B2,
      X2 : Observable[B2, ?] : Indexable[N2, ?] : Positionable,
      N2,
      B3 : Indexable[N3, ?],
      X3 : Observable[B3, ?] : Indexable[N3, ?] : Tickable : Joinable,
      N3] =
    timerAndGeoentities[B1, X1, F, B2, X2, N2] |>| geofences[F, B3, X3, N3].in
}
