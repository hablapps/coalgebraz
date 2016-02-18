package org.hablapps.geofence

import scalaz._, Scalaz._

import org.hablapps.coalgebraz._, Coalgebraz._
import wrap.dsl._

import state._

trait Routing { this: state.State =>

  implicit def routeInGeofences[
    F[_] : Functor : Mappable, B1, B2 : Indexable[N2, ?] : Joinable, N1, N2]
      : Router[
        F[B2],
        ClockOut \/ IndexOut[GeolocationOut, B1, N1],
        IndexIn[ClockOut \/ GeofenceIn, B2, N2]] = obs => {
    case -\/(Tick) => toMappableOps(obs).keys.map(k => (k, Tick.left).wrap)
    case \/-(WrapOut((n, out))) => out match {
      // case Moved(pos) => {
      //   def f(cn: Boolean, cv: Boolean, e: String => ClockOut \/ GeofenceIn) = {
      //     obs.toList.filter { t =>
      //       (t._2.elements contains n) == cn && t._2.covers(pos) == cv
      //     }.map(t => (t._1, e(n)).wrap)
      //   }
      //   f(true, false, n => Leave(n).right) ++
      //     f(false, true, n => Join(n).right)
      // }
      case Halted => obs.filter(_.contains(n.toString)).keys.map { k =>
        (k, Leave(n.toString).right).wrap
      }
    }
    case _ => List.empty
  }
}
