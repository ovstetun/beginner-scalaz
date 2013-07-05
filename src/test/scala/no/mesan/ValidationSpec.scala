package no.mesan

import scalaz._
import scalaz.Scalaz._

import org.specs2.mutable.Specification

class ValidationSpec extends Specification {
  "Validation" should {
    "be of types" in {
      val e: Equal[Validation[String, Int]] = Equal[Validation[String, Int]]
      e must not beNull

      val sg: Semigroup[Validation[String, Int]] = Semigroup[Validation[String, Int]]
      sg must not beNull

      val m: Monoid[Validation[String, Int]] = Monoid[Validation[String, Int]]
      m must not beNull

      val a: Applicative[({type l[a] = Validation[String, a]})#l]  = Validation.ValidationApplicative[String]
      a must not beNull

      val t: Traverse[({type l[a] = Validation[String, a]})#l] = Traverse[({type l[a] = Validation[String, a]})#l]
      t must not beNull
    }
    "be experimentable" in {
      val x = "feil".failNel[Int]
      x match {
        case Success(a) => "success"
        case Failure(e) => "fail"
      }
      val res = x.fold(
        fail = {
          case x => "f"
        },
        succ = {
          case x => "s"
        }
      )

      1.failNel must beAnInstanceOf[ValidationNel[Nothing, Int]]
      1.failNel[String] must beAnInstanceOf[ValidationNel[String, Int]]
    }
    "be flatMappable" in {
      1.success[String] must_== Success[String, Int](1)
      1.successNel[String] must_== Success[NonEmptyList[String], Int](1)
      1.success[String] map {_*2} must_== 2.success[String]

      "Too young!".failNel[Int] must_== Failure(NonEmptyList("Too young!"))

      case class User(username:String)
      def addUser(username: String): ValidationNel[String, User] = {
        findUser(username) match {
          case Some(_) => s"Username '${username}' taken!".failNel
          case _ => User(username).success
        }
      }
      def findUser(username: String): Option[User] = {
        (username === "ovstetun") ? User("ovstetun").some | scalaz.std.option.none
      }

      addUser("per") must_== Success(User("per"))
      addUser("ovstetun") must_== Failure(NonEmptyList("Username 'ovstetun' taken!"))
    }
  }
}
