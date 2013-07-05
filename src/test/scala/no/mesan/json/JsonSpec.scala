package no.mesan.json

import org.specs2.mutable.Specification

import scalaz._, Scalaz._

import org.json4s.scalaz._
import org.json4s.scalaz.JsonScalaz._
import org.json4s._
import org.json4s.native.Serialization._
import org.json4s.native.JsonMethods._
import no.mesan.test.Person
import org.json4s.native.JsonParser

import scala.language.reflectiveCalls

class JsonSpec extends Specification {
  implicit val formats = DefaultFormats

  type EitherNel[+A] = NonEmptyList[Error] \/ A
  def validate[A: JSONR](name: String) = Kleisli(field[A](name)).mapK[EitherNel, A](_.disjunction)
//  implicit def makeValidation[A](v: Kleisli[EitherNel, JValue, A]) = (v.run _).andThen(_.validation)


  val json = """{"name": "Petter", "age": 22}"""
  val j = JsonParser.parse(json)
  val petter = Person("Petter", 22)

  "pure json4s" should {
    "be able to read well-formed" in {
      val json = """{"name": "Petter", "age": 22}"""
      read[Person](json) must_== petter
      read[Person](json) must_== Person("Petter", 22)
    }
    "fail with exception when missing field" in {
      val json = """{"name":"Petter Alderlos"}"""
      read[Person](json) must throwA[MappingException]
    }
  }
  "json4s with scalaz" should {
    case class P(x:Int, y:Int)
    val n = P.applyJSON(field[Int]("x"), field[Int]("y"))

    def min(x: Int): Int => Result[Int] = (y: Int) =>
      if (y < x) Fail("min", y + " < " + x) else y.success
    def max(x: Int): Int => Result[Int] = (y: Int) =>
      if (y > x) Fail("max", y + " > " + x) else y.success

    val x = field[Int]("age") _
    // Creates a function JValue => Result[Person]
    val pj = Person.applyJSON(field[String]("name"), field[Int]("age"))

    "be able to read well-formed" in {
      Person.applyJSON(field[String]("name"), field[Int]("age"))(j) must_== Success(petter)

      implicit val xy: JSONR[Person] = Person.applyJSON(field[String]("name"), field[Int]("age"))
      fromJSON[Person](j) must_== Success(petter)

      val json = native.JsonParser.parse( """ [1,2,3] """)
      fromJSON[(Int, Int, Int)](json) must_== Success(1, 2, 3)
    }
//    "using validation" in {
//      pending
//      def mmin(x:Int) = (i:Int) => min(x)(i).disjunction
//      def mmax(x:Int) = (i:Int) => max(x)(i).disjunction
//
//      val ageValidator1 = Kleisli(field[Int]("age")).mapK[EitherNel, Int](_.disjunction)
////      val ageValidator1 = validate[Int]("age")
//      val ageValidator2 = {i: Int => (min(18)(i) |@| max(60)(i))((a, _) => a).disjunction}
//
//      val x: Kleisli[EitherNel, JValue, Int] = validate[Int]("age") >==> mmin(18) >==> mmax(60)
//
//      val ageValidator = ((ageValidator1 >==> ageValidator2).run _).andThen(_.validation)
//
//      val person2 = Person.applyJSON(field[String]("name"), ageValidator) //validate[Int]("age"))
//
//      val person3 = Person.applyJSON(field[String]("name"), validate[Int]("age") >==> mmin(18) >==> mmax(60))
//
//      person2(j) must_== Success(Person("Petter", 22))
//      person2(parse( """{"name":"Petter Alderlos", "age":17 """)) must_== Success(Person("Petter", 22))
//      person3(j) must_== Success(Person("Petter", 22))
//
//      implicit val personR: JSONR[Person] = person3
//
//      fromJSON[Person](j) must_== Success(Person("Petter", 22))
      // doesn't fail the way I expected
//      fromJSON[Person](parse( """{"name":"Petter Alderlos"}""")) must_== Failure(NonEmptyList(NoSuchFieldError("age", JObject(List(("name", JString("Petter Alderlos")))))))
//      fromJSON[Person](parse( """{"name":"Petter Alderlos", "age":17 """)) must_== Failure(NonEmptyList(NoSuchFieldError("age", JObject(List(("name", JString("Petter Alderlos")))))))
//      fromJSON[Person](parse( """{"name":"Petter Alderlos", "age":61 """)) must_== Failure(NonEmptyList(NoSuchFieldError("age", JObject(List(("name", JString("Petter Alderlos")))))))
//    }
    "using validation" in {
      val avJoe = parse( """ {"name":"Average Joe","age":34,"address":{"street": "Manhattan 2", "zip": "00223" }} """)
      val youngJoe = parse( """ {"name":"Young Joe","age":17,"address":{"street": "Manhattan 2", "zip": "00223" }} """)
      val oldJoe = parse( """ {"name":"Old Joe","age":61,"address":{"street": "Manhattan 2", "zip": "00223" }} """)

      def min(x: Int): Int => Result[Int] = (y: Int) =>
        if (y < x) Fail("min", y + " < " + x) else y.success

      def max(x: Int): Int => Result[Int] = (y: Int) =>
        if (y > x) Fail("max", y + " > " + x) else y.success

      // these types have been commited to json4s by me
      type EitherNel[+A] = NonEmptyList[Error] \/ A
      def validate[A: JSONR](name: String) = Kleisli(field[A](name)).mapK[EitherNel, A](_.disjunction)
      implicit def function2EitherNel[A](f: A => Result[A]): (A => EitherNel[A]) = (a: A) => f(a).disjunction
      implicit def kleisli2Result[A](v: Kleisli[EitherNel, JValue, A]): JValue => Result[A] = (v.run _).andThen(_.validation)

      val ageValidator1 = validate[Int]("age")
      val ageValidator2 = {i: Int => (min(18)(i) |@| max(60)(i))((a, _) => a).disjunction}

      val person2 = Person.applyJSON(field[String]("name"), ageValidator1 >==> ageValidator2)
      person2(avJoe) must_== Success(Person("Average Joe", 34))
      person2(youngJoe) must_== Failure(NonEmptyList(UncategorizedError("min", "17 < 18", List())))
      person2(oldJoe) must_== Failure(NonEmptyList(UncategorizedError("max", "61 > 60", List())))

      val person3 = Person.applyJSON(field[String]("name"), validate[Int]("age") >==> min(18) >==> max(60))
      person3(avJoe) must_== Success(Person("Average Joe", 34))
      person3(youngJoe) must_== Failure(NonEmptyList(UncategorizedError("min", "17 < 18", List())))
      person3(oldJoe) must_== Failure(NonEmptyList(UncategorizedError("max", "61 > 60", List())))

      implicit val personR: JSONR[Person] = person3
      fromJSON[Person](avJoe) must_== Success(Person("Average Joe", 34))
      fromJSON[Person](youngJoe) must_== Failure(NonEmptyList(UncategorizedError("min", "17 < 18", List())))
      fromJSON[Person](oldJoe) must_== Failure(NonEmptyList(UncategorizedError("max", "61 > 60", List())))

      fromJSON[Person](parse("""{"age":17}""")) must_== Failure(NonEmptyList(
        NoSuchFieldError("name", JObject(List(("age", JInt(17))))),
        UncategorizedError("min", "17 < 18", List())))
    }
    "fail with Error" in {
      val j2 = JsonParser.parse("{}")
      implicit val xy: JSONR[Person] = Person.applyJSON(field[String]("name"), field[Int]("age"))
      fromJSON[Person](j2) must_== Failure(NonEmptyList(NoSuchFieldError("name", JObject(List())), NoSuchFieldError("age", JObject(List()))))
    }
  }
}
