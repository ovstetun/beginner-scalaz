package no.mesan.test

import scala.util.Properties

import unfiltered.filter.Plan
import unfiltered.request._
import unfiltered.response._

import scalaz._
import Scalaz._

import org.json4s.scalaz.JsonScalaz._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.Serialization._
import org.json4s.native.JsonParser._

import scala.language.{higherKinds, reflectiveCalls, implicitConversions}


object Tester extends App {
  val port = Properties.envOrElse("PORT", "1337").toInt

  unfiltered.jetty.Http(port)
    .filter(new TestPlan)
    .run(_ => println(s"Server started on port:${port}"))
}

case class Person(name: String, age: Int)

trait PersonDAO {
  def add(p:Person): Int
}

object Validate {
  def notNull(x: Any) = if (x == null) throw new IllegalArgumentException
  def isTrue(x: Boolean) = if (!x) throw new IllegalArgumentException
}

class TestPlan extends Plan {
  // code needed for implicits in json-validation to work
  type EitherNel[+a] = NonEmptyList[Error] \/ a
  def validate[A: JSONR](name: String) = Kleisli(field[A](name)).mapK[EitherNel, A](_.disjunction)
  implicit def function2EitherNel[A](f: A => Result[A]): (A => EitherNel[A]) = (a: A) => f(a).disjunction
  implicit def kleisli2Result[A](v: Kleisli[EitherNel, JValue, A]): JValue => Result[A] = (v.run _).andThen(_.validation)
  // end code for json-validation

  implicit val formats = DefaultFormats

  def min(x: Int): Int => Result[Int] = (y: Int) =>
    if (y < x) Fail("min", y + " < " + x) else y.success
  def max(x: Int): Int => Result[Int] = (y: Int) =>
    if (y > x) Fail("max", y + " > " + x) else y.success

  implicit val personR: JSONR[Person] = Person.applyJSON(field[String]("name"), validate[Int]("age") >==> min(18) >==> max(60))

  val personer: PersonDAO = new PersonDAO {
    def add(p: Person): Int = {
      Validate.notNull(p.name)
      Validate.isTrue(p.age > 18 && p.age < 60)
      1
    }
  }

  def intent = {
    case GET(Path("/")) => Ok
    case POST(Path("/")) => ???
    case req @ POST(Path("/person")) =>  {
      val x = Body.string(req)
      val p: Person = read[Person](x)
      val id: Int = personer.add(p)

      Created ~> Json("id" -> id)
    }
    case req @ POST(Path("/personz")) => {
      val x = Body.string(req)
      val p: ValidationNel[Error, Person] = fromJSON[Person](parse(x))
      val id: ValidationNel[Error, Int] = p map personer.add

      id match {
        case Success(i) => Created ~> Json("id" -> i)
        case Failure(errors) => Forbidden ~> Json("errors" -> jsonErrors(errors).toList)
      }
    }
  }

  def jsonErrors(errors: NonEmptyList[Error]): NonEmptyList[String] = {
    def jsonError(error: Error) = error match {
      case NoSuchFieldError(name, _) => "Missing field: " + name
      case UncategorizedError(name, desc, _) => "Validation failed for field: " + name + ". Error message: " + desc
      case UnexpectedJSONError(_,_) => "Unexpected error: "
    }
    errors map jsonError
  }
}
