package no.mesan

import org.specs2.mutable.Specification
import scalaz._
import scalaz.std.int._

class OrderSpec extends Specification {
  "Order typeclass" should {
    "have a Int-instance" in {
      Order[Int].order(1, 2) must_== Ordering.LT
      implicitly(Order[Int])(1, 2) must_== Ordering.LT
    }
  }
  import scalaz.syntax.order._
  "Order with syntax" should {
    "work for ints" in {
      1 <   2 must beTrue
      1 lt  2 must beTrue
      1 <   2.0 must beTrue // is actually using scala standard comp
      // 1 lt  2.0 must beTrue // fails to compile, uncompatible types
      1 <=  1 must beTrue
      1 lte 1 must beTrue
      1 lte 1 must beTrue

      1 <=  2 must beTrue
      1 lte 2 must beTrue

      2 >=  1 must beTrue
      2 gte 1 must beTrue

      1 max 2 must_== 2
      1 min 2 must_== 1

      1 ?|? 2 must_== Ordering.LT
      2 ?|? 1 must_== Ordering.GT
      1 ?|? 1 must_== Ordering.EQ

      import scalaz.{CaseInsensitive => CI}
      CI("a") lt CI("b") must beTrue
    }
  }
  "Creating my own order instance" should {
    case class Person(name:String, age:Int)
    implicit val cOrder = new Order[Person] {
      def order(x: Person, y: Person): Ordering = x.age ?|? y.age
    }
    "pending" in {
      Person("Per", 22) lt Person("Pål", 25) must beTrue
    }
  }
}
