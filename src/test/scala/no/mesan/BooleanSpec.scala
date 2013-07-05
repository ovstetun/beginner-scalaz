package no.mesan

import org.specs2.mutable._

import scalaz.std.boolean._
import scalaz.syntax.std.boolean._
import scalaz.std.anyVal.intInstance

class BooleanSpec extends Specification {

  " /\\ (conjunction)" should {
    "be true for true, true" in {
      true && true must_== true
      true /\ true must_== true
      true /\ true must beTrue
      true ∧ true must beTrue // unicode-version
    }
    "be false for any one false" in {
      true && false must_== false
      true /\ false must_== false

      true /\ false must beFalse
      false /\ true must beFalse
      false /\ false must beFalse
    }
  }
  " \\/ (disjunction" should {
    "be true for any one true" in {
      true \/ false must_== true
      true || false must_== true

      true \/ true must beTrue
      true \/ true must beTrue
      false \/ true must beTrue
      true \/ false must beTrue
    }
    "be false for both false" in {
      false \/ false must beFalse
      false ∨ false must beFalse // unicode
    }
  }

  "? (ternary)" should {
    "evaluate to first argument for true" in {
      (if (true) "a" else "b") must_== "a"
      true ? "a" | "b" must_== "a"
    }
    "evaluate to second argument for false" in {
      false ? "a" | "b" must_== "b"
      (if (false) "a" else "b") must_== "b"
    }
  }
  "option" should {
    "return arg as option on true" in {
      true option "a" must_== Some("a")
    }
    "return none on false" in {
      false option "a" must_== (None:Option[String])
    }
  }
  "??" should {
    "return given if true" in {
      true ?? 42 must_== 42
    }
    "return zero for false" in {
      false ?? 42 must_== 0
    }
  }

}
