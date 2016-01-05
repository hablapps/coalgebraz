package org.hablapps.coalgebraz.candy

import scalaz._, Scalaz._, Isomorphism.<=>

import org.hablapps.coalgebraz._

import Coalgebra._, CoentityOps._

object Cocandy {

  val cokey: Coentity[Void, Void, String, String] = constant[String]

  val coflavour: Coistore[FlavourIn, Flavour, Flavour] =
    s => IStore(s, _ match {
      case Become(flavour) => flavour
    })

  val coposition: Coistore[PositionIn, (Int, Int), (Int, Int)] = {
    case s@(x, y) => IStore(s, _ match {
      case OverX(f) => (f(x), y)
      case OverY(f) => (x, f(y))
    })
  }

  implicit val isoCandy = new (((String, Flavour), (Int, Int)) <=> Candy) {
    val to: (((String, Flavour), (Int, Int))) => Candy = {
      case ((key, fla), pos) => Candy(key, fla, pos)
    }
    val from: Candy => ((String, Flavour), (Int, Int)) = {
      case Candy(key, fla, pos) => ((key, fla), pos)
    }
  }

  // TODO
  // 1) `extend` input with a new type `CandyIn2`
  // 2) output should be `CandyOut`
  val cocandy: Coentity[CandyIn1, Void, Candy, Candy] =
    (cokey |+| coflavour |+| coposition)
      .withState[Candy]
      .withObservable[Candy]
      .routeIn[CandyIn1]((_, ci) => ci match {
        case Fall(n)    => List(OverY(_ + n).right)
        case Slide(dir) => List(dir.toPositionIn.right)
        case Mutate(fl) => List(Become(fl).left)
      })
}
