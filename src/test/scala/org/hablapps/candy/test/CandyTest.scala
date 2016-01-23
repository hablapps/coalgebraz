package org.hablapps.candy.test

import scala.util.Random

import org.scalatest._

import org.hablapps.coalgebraz._, EntityOps._
import org.hablapps.coalgebraz.test._, PropFramework._

import org.hablapps.candy._, Cocandy._
import Nat.Syntax._

class CandyTest extends FunSpec with ShouldMatchers {

  it("counter should be always equal or greater than zero") {

    val prop1: EntityProp[Nat] = EntityProp.always(_ >= 0)

    satisfied(
      game(1000).withObservable[Nat](To(_._2)))(
      prop1,
      (Board(8, List(Candy("one", Lemon, (1, 1)))), new Random(), 0),
      List(
        Interchange((1, 1), South),
        Interchange((2, 1), East),
        Interchange((2, 2), North),
        Interchange((2, 3), West))) shouldBe DontKnow
  }

  it("counter in next state should be equal or greater than zero") {

    val prop2: EntityProp[Nat] = EntityProp.next(_ >= 0)

    satisfied(
      game(1000).withObservable[Nat](To(_._2)))(
      prop2,
      (Board(8, List(Candy("one", Lemon, (1, 1)))), new Random(), 0),
      List(
        Interchange((1, 1), South),
        Interchange((2, 1), East),
        Interchange((2, 2), North),
        Interchange((2, 3), West))) shouldBe Yes
  }

  it("counter in next state should be always equal or greater than current") {
    pending
  }
}
