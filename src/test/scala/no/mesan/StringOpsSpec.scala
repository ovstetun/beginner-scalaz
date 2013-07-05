package no.mesan

import org.specs2.mutable.Specification

import scalaz._
import org.specs2.scalaz.ValidationMatchers

class StringOpsSpec extends Specification with ValidationMatchers {
  "Standard String" should {
    "fail to parse as int" in {
      "fortytwo".toInt must throwA[NumberFormatException]
    }
    "parse valid input to int" in {
      "42".toInt must_== 42
    }
  }
  "StringOps String" should {
    import scalaz.std.string.stringSyntax._

    "parse invalid int to Failure" in {
      "fortytwo".parseInt must beFailing
    }
    "parse valid input to Success" in {
      "42".parseInt must beSuccessful(42)
      "42".parseInt must_== Success(42)
    }
    "encode a string" in {
      "using explicit CharSet" in {
        "æøå".encode(CharSet.ISO8859) must_== Array(-26, -8, -27)
      }
      "using implicit CharSet" in {
        implicit val e = CharSet.UTF8
        "æøå".encode must_== Array(-61, -90, -61, -72, -61, -91)
      }
    }
    "convert a string to a NEL" in {
      "".charsNel must beNone
      "".charsNel(NonEmptyList('f','a','i','l')) must_== NonEmptyList('f', 'a', 'i', 'l')
      "asdf".charsNel must beSome(NonEmptyList('a','s','d','f'))
    }
  }
}
