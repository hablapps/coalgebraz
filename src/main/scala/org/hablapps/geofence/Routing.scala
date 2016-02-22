package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._, Coalgebraz._
import wrap.dsl._

import state._

trait Routing { this: state.State =>

  implicit def routeInGeofences[
    F[_] : Functor : Mappable, B1, B2 : Indexable[N2, ?] : Joinable : Coverable, N1, N2]
      : Router[
        F[B2],
        ClockOut \/ IndexOut[GeolocationOut, B1, N1],
        IndexIn[ClockOut \/ GeofenceIn, B2, N2]] = obs => {
    case -\/(Tick) => toMappableOps(obs).keys.map(k => (k, Tick.left).wrap)
    case \/-(WrapOut((n, out))) => out match {
      case Moved(pos) => {
        def f(cn: Boolean, cv: Boolean, e: String => ClockOut \/ GeofenceIn) = {
          obs.filter { (k, v) =>
            (v.contains(n.toString) == cn) && (v.covers(pos) == cv)
          }.keys.map(k => (k, e(n.toString)).wrap)
        }
        f(true, false, n => Leave(n).right) ++
          f(false, true, n => Join(n).right)
      }
      case Halted => obs.filter((_, v) => v.contains(n.toString)).keys.map { k =>
        (k, Leave(n.toString).right).wrap
      }
    }
    case _ => List.empty
  }
}
