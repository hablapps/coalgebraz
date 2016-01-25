package org.hablapps.candy.test

import scala.util.Random

import org.scalatest._

import org.hablapps.coalgebraz._, EntityOps._
import org.hablapps.coalgebraz.test._, PropFramework._

import org.hablapps.candy._, Cocandy._
import Nat.Syntax._

import EntityProp._

class CandyTest extends FunSpec with ShouldMatchers {

  // Just a game that takes only the counter (ignoring board) as observation.
  val co = level(100).withObservable[Nat](To(_._2))

  it("counter should be always equal or greater than zero") {
    val prop: EntityProp[Nat] = always(n => just(n >= 0))
    prop undecidableBy co
  }

  it("counter in next state should be equal or greater than zero") {
    val prop: EntityProp[Nat] = next(n => just(n >= 0))
    prop assertedBy co
  }

  it("counter in next state should be always equal or greater than current") {
    val prop: EntityProp[Nat] = always(c1 => next(c2 => just(c2 >= c1)))
    prop undecidableBy co
  }

  // XXX: this is just an ad hoc prototype! A complete version should integrate
  // our properties with ScalaCheck ones, taking the opportunity to exploit
  // state and input generators.
  implicit class SatisfiedHelper(prop: EntityProp[Nat]) {

    val s = (Board(8, List(Candy("one", Lemon, (1, 1)))), new Random(), Nat(0))
    val input = List(
      Interchange((1, 1), South),
      Interchange((2, 1), East),
      Interchange((2, 2), North),
      Interchange((2, 3), West))

    private def checkedBy(
        co: Entity[BoardIn, CounterOut, Nat, (Board, Random, Nat)],
        exp: Satisfied) {
      satisfied(co)(prop, s, input) shouldBe exp
    }

    def assertedBy(
        co: Entity[BoardIn, CounterOut, Nat, (Board, Random, Nat)]) =
      checkedBy(co, Yes)

    def undecidableBy(
        co: Entity[BoardIn, CounterOut, Nat, (Board, Random, Nat)]) =
      checkedBy(co, DontKnow)

    def negatedBy(
        co: Entity[BoardIn, CounterOut, Nat, (Board, Random, Nat)]) =
      checkedBy(co, No)
  }
}
