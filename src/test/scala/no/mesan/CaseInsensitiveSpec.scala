package no.mesan

import org.specs2.mutable.Specification

import scalaz.{CaseInsensitive => CI, FoldCase}
import CI._

class CaseInsensitiveSpec extends Specification {
  "CaseInsensitive" should {
    "compare strings with case insensitivity" in {
      CI("TmO") must_== CI("tmo")
      CI("TmO").foldedCase must_== "tmo"
    }
    "behave as expected when put in a map" in {
      val m = Map(CI("TmO") -> 1, CI("mEK") -> 2)
      m(CI("tMo")) must_== 1
    }
    "be reasonable in a set" in {
      val s = Set(CI("TmO"), CI("tmo"))
      s.size must_== 1
      s(CI("tmO")) must beTrue
    }
    "create own FoldCase" in {
//      CI('A') must_== CI('a') //doesn't compile => needs implicit on next line
      implicit val charFoldCase = new FoldCase[Char] {
        def foldCase(a: Char): Char = a.toLower
      }
      CI('A') must_== CI('a')

      case class Person(name: String)
      implicit val personFoldCase = new FoldCase[Person] {
        def foldCase(p: Person): Person = Person(CI(p.name).foldedCase)
      }
      CI(Person("Petter")) must_== CI(Person("peTTer"))
    }

  }
}
