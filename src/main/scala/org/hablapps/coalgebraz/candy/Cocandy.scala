package org.hablapps.coalgebraz.candy

import scala.util.Random

import scalaz._, Scalaz._, Isomorphism.<=>

import org.hablapps.coalgebraz._
import org.hablapps.coalgebraz.Store
import Coalgebraz._, CoentityOps._
import Sq.someOrNone
import Nat._, Nat.Syntax._

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

  val cocandy1: Coentity[CandyIn1, Void, Candy, Candy] =
    (cokey |+| coflavour |+| coposition)
      .withState[Candy]
      .withObservable[Candy]
      .routeIn(_ => _ match {
        case Fall(n)    => List(OverY(_ + n).right)
        case Slide(dir) => List(dir.toPositionIn.right)
        case Mutate(fl) => List(Become(fl).left)
      })

  val cocandy2: Coentity[CandyIn2, CandyOut, Candy, Candy] = {
    case c: Candy => Entity(c, _ => (List(ByeCandy), None))
  }

  val cocandy: Coentity[CandyIn, CandyOut, Candy, Candy] =
    cocandy1 /+\ cocandy2

  val cocandies: CoentitySeq[CandyIn, CandyOut, Candy, Candy] =
    cocandy.toCoseq

  // XXX: side-effecting random. It'll be nice to use a pure one!
  val corandom: Coistream[Int, Random] = rnd => IStream(rnd.nextInt, rnd)

  // A counter system which will stop right after overtaking the passed limit.
  def cocounter(limit: Int): Costore[CounterIn, Int, Nat] = { x =>
    Store(x.asInt, _ match {
      case Increase(n) => {
        val x2 = x + Nat(n)
        if (x2.asInt > limit) None else Option(x2)
      }
      case Decrease(n) => Option(x - Nat(n))
    })
  }
}
