package coalgebraz.example.geofence

import scalaz._, Scalaz._

import coalgebraz._, Coalgebraz._

import state._

trait Routing { this: state.State =>

  implicit def routeInGeofences[
    F[_, _] : Mappable, B1, B2 : Joinable : Coverable, N1, N2](implicit
      ev0: Functor[F[N2, ?]])
      : Router[
        F[N2, B2],
        ClockOut \/ IndexOut[GeolocationOut, B1, N1],
        IndexIn[ClockOut \/ GeofenceIn, B2, N2]] = obs => {
    case -\/(Tick) => obs.map(_ => Tick.left).toList.map(_.wrap)
    case \/-(WrapOut((n, out))) => out match {
      case Moved(pos) => {
        def f(cn: Boolean, cv: Boolean, e: String => ClockOut \/ GeofenceIn) = {
          obs.filter { kv =>
            (kv._2.contains(n.toString) == cn) && (kv._2.covers(pos) == cv)
          }.map(_ => e(n.toString)).toList.map(_.wrap)
        }
        f(true, false, n => Leave(n).right) ++
          f(false, true, n => Join(n).right)
      }
      case Halted => obs.filter(kv => kv._2.contains(n.toString)).map { _ =>
        Leave(n.toString).right
      }.toList.map(_.wrap)
    }
    case _ => List.empty
  }
}
