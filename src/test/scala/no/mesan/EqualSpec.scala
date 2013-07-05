package no.mesan

import org.specs2.mutable.Specification

import scalaz.syntax.equal._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.Equal
import org.specs2.matcher.ScalaCheckMatchers
import org.scalacheck.{Gen, Arbitrary}
import scalaz.scalacheck.ScalazProperties._

//import scalaz._, Scalaz._

class EqualSpec extends Specification with ScalaCheckMatchers {

  "Equallaw" should {
    "pass" in {
//      implicit val arbInt = for { x <- Gen.choose(0, 1000) } yield x
      implicit val arbInt = Arbitrary.arbitrary[Int]
      check(equal.laws[Int])
    }
    "pass for my own equal" in {
      case class Car(model: String)
      implicit object carEqual extends Equal[Car] {
        def equal(a1: Car, a2: Car): Boolean = a1.model == a2.model
      }
      implicit val arbitraryVec = Arbitrary {
        for {
          (x) <- Arbitrary.arbitrary[String]
        } yield Car(x)
      }

      check(equal.laws[Car])
    }
  }

  "Equal typeclass direct usage" should {
    "compare ints to ints" in {
      Equal[Int].equal(1, 1) must beTrue
      Equal[Int].equal(1, 2) must beFalse
//      Equal[Int].equal(1, "1") // doesn't compile - of course
    }
    "contramap string to int" in {
      Equal[Int].contramap((x: String) => x.toInt).equal("1", "1")
      Equal[Int].contramap[String](_.toInt).equal("1", "1")
    }
  }

  "===" should {
    "compare ints to int" in {
      1 == 1 must_== true
      1 == 1.0 must_== true
      class Person(name: String)

      val x: Int = 1
      val y: Person = new Person("Petter")
      x == y must_== false
      y == x must_== false

      1 === 1 must beTrue
      1 === 2 must beFalse
//      1 === 1L must beTrue // ikke riktig å sammenligne Int <--> Long
    }
    "compare string to string" in {
      "a" === "a" must beTrue
      "a" === "b" must beFalse
    }
    "compare case classes" in {
      case class MyInt(int: Int)
      implicit val myIntEquals = Equal.equalA[MyInt]

      MyInt(1) === MyInt(1) must beTrue
      MyInt(1) === MyInt(2) must beFalse
    }
  }
  "=!=" should {
    "compare int to int" in {
      1 =/= 2 must beTrue
      1 =/= 1 must beFalse
    }
    "compare string to string" in {
      "a" =/= "a" must beFalse
      "a" =/= "b" must beTrue
    }
  }

  "Equals på datatype" should {
    sealed trait TrafficLight
    case object Red extends TrafficLight
    case object Yellow extends TrafficLight
    case object Green extends TrafficLight
    implicit val TrafficLightEqual: Equal[TrafficLight] = Equal.equalA[TrafficLight]

    "be comparable only when typed as supertype" in {
//      Red === Yellow // doesn't complie due to nonvariant subtyping in Equal[T]
      (Red: TrafficLight) === (Red: TrafficLight) must beTrue
      (Red: TrafficLight) === (Yellow: TrafficLight) must beFalse

      val r: TrafficLight = Red
      val y: TrafficLight = Yellow
      r === r must beTrue
      r === y must beFalse
    }
    "should pass the laws" in {
      implicit val arbLight: Arbitrary[TrafficLight] = Arbitrary {Gen.oneOf(Red, Yellow, Green)}
      check(equal.laws[TrafficLight])
    }
  }

  "Egendefinert Equal for et enkelt klassehierarki" should {
    abstract class Car(val model: String)
    case class SimpleCar(override val model: String) extends Car(model)
    case class ComplexCar(override val model: String) extends Car(model)

    val sCar: Car = SimpleCar("a")
    val sCar2: Car = SimpleCar("a")
    val cCar: Car = ComplexCar("a")

    "kunne lages med Equal.equalA" in {
      implicit val x1 = Equal.equalA[Car]

      sCar === sCar2 must beTrue
      sCar === cCar must beFalse
    }
    "kunne lages fra scratch" in {
      implicit val carEqual = new Equal[Car] {
        def equal(a1: Car, a2: Car): Boolean = a1.model == a2.model
      }

      sCar === cCar must beTrue
    }
    "kunne lages basert på Equal-instans" in {
      implicit val carEquals : Equal[Car] = Equal.equalA[Car]
      implicit val simpleCarEquals : Equal[SimpleCar] = Equal.equalBy((a: SimpleCar) => a.asInstanceOf[Car])

      SimpleCar("a") === SimpleCar("a") must beTrue
    }
  }
}
