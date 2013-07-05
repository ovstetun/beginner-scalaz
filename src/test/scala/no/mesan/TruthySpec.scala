package no.mesan

import org.specs2.mutable.Specification

class TruthySpec extends Specification {
  "Truthy" should {
    "provide an Int instance" in {
      Truthy[Int] must not beNull

      Truthy[Int] must beAnInstanceOf[Truthy[Int]]

      Truthy[Int].truthy(0) must beFalse
      Truthy[Int].truthy(1) must beTrue
    }
    "provide a List instance" in {
      Truthy[List[Int]] must not beNull

      Truthy[List[String]] must not beNull

      Truthy[List[String]] must_!= Truthy[List[Int]]
    }
    "provide lifting via implicits" in {
      import ToTruthyOps._

      1.truthy must beTrue
      0.truthy must beFalse

      Nil.truthy must beFalse
      List(1).truthy must beTrue
    }
  }
  "iffy" should {
    "work" in {
      import ToTruthyOps._
      iffy(Nil) {"YEAH!"} {"NO!"} must_== "NO!"
      iffy(1)("Ja!")("Neeei!") must_== "Ja!"
    }
  }
}
