package coalgebraz.example.candycrush.test

import scala.util.Random

import org.scalatest._

import coalgebraz._, Coalgebraz._
import coalgebraz.test._, PropFramework._

import coalgebraz.example.candycrush._, CandyCrush._
import Nat.Syntax._

import EntityProp._

class CandyCrushTest extends FunSpec with ShouldMatchers {

  // Just a game that takes only the counter (ignoring board) as observation.
  val co = level(100).observe[Nat](To(_._2))

  it("counter should be always equal or greater than zero") {
    val prop: EntityProp[Nat] = EntityProp.always(n => just(n >= 0))
    prop undecidableBy co
  }

  it("counter in next state should be equal or greater than zero") {
    val prop: EntityProp[Nat] = EntityProp.next(n => just(n >= 0))
    prop assertedBy co
  }

  it("counter in next state should be always equal or greater than current") {
    val prop: EntityProp[Nat] =
      EntityProp.always(c1 => EntityProp.next(c2 => just(c2 >= c1)))
    prop undecidableBy co
  }

  // XXX: this is just an ad hoc prototype! A complete version should integrate
  // our properties with ScalaCheck ones, taking the opportunity to exploit
  // state and input generators.
  implicit class SatisfiedHelper(prop: EntityProp[Nat]) {

    val s = (
      Board(8, Map("one" -> Candy("one", Lemon, (1, 1)))),
      new Random(),
      Nat(0))

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
