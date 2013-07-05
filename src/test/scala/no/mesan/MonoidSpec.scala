package no.mesan

import org.specs2.mutable.Specification
import scalaz.std.{option => o}

//import scalaz.{Monoid, Semigroup}
//import scalaz.std.int._
//import scalaz.std.AllInstances.intInstance
//import scalaz.std.AllInstances.listInstance
//import scalaz.syntax.monoid._
import scalaz._, Scalaz._
import Tags._

class MonoidSpec  extends Specification {
  "Monoid" should {
    "combine ints" in {
      1 |+| 2 must_== 3
      Multiplication(1) |+| Multiplication(4) must_== 4
    }
    "multiply ints and strings" in {
      3 multiply 5 must_== 15
      "a" multiply 3 must_== "aaa"
    }
    "ifEmpty" in {
      (0.ifEmpty("Empty")("NonEmpty")) must_== "Empty"
      (1.ifEmpty("Empty")("NonEmpty")) must_== "NonEmpty"
    }
    "combine lists by concatenation" in {
      List(1,2) |+| List(3,4) must_== List(1,2,3,4)
    }
    "combine maps" in {
      val m1 = Map(1 -> 1, 2 -> 3)
      val m2 = Map(1 -> 4, 3 -> 6)
      Map(1 -> 1, 2 -> 3) |+| Map(1 -> 4, 3 -> 6) must_== Map(1 -> 5, 3 -> 6, 2 -> 3)

      m1 |+| m2 must_== Map(1 -> 5, 3 -> 6, 2 -> 3)
    }
    "combine tuples" in {
      (1,2) |+| (3,4) must_== (4,6)
    }
    "combine options" in {
      1.some |+| 2.some must_== 3.some
      (1.some |+| o.none[Int]) must_== 1.some
      (o.none[Int] |+| 2.some) must_== 2.some

      (1.some, 2.some) |+| (3.some, o.none[Int]) must_== (4.some, 2.some)
    }
    "or zero" in {
      ~o.none[Int] must_== 0
      ~3.some must_== 3
    }
  }
}
