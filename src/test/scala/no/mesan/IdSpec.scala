package no.mesan

import org.specs2.mutable.Specification

import scalaz._
import Scalaz._

class IdSpec extends Specification {
  "Id" should {
    "wrap an object" in {
      (1: Id[Int]) must_== 1
    }
    "?? is like orElse" in {
      ("a" ?? "b") must_== "a"

      val x: String = null
      (x ?? "asdf") must_== "asdf"
    }
    "|> applies to a function" in {
      def f(i:Int) = i + 1
      (1 |> f) must_== 2

      1 + 2 + 3 |> {_ * 6} must_== 36
    }
    "wrapNel turns into a nonempty list" in {
      1.wrapNel must_== NonEmptyList(1)
    }
  }
}
