package no.mesan

import scalaz.syntax.index._
import scalaz.syntax.std.option._
import scalaz.std.{option => o}
import scalaz.std.list, list._
import org.specs2.mutable.Specification
import java.lang.IndexOutOfBoundsException

class IndexSpec extends Specification {

  val l = List(1,2,3,4)

  "Index" should {
    "return instance from list" in {
      l index 0 must_== 1.some
      l index 2 must_== 3.some

      l(0) must_== 1
      l(2) must_== 3
    }
    "return none for indexoutofbounds" in {
      l index -1 must_== o.none[Int]
      l index 4 must_== o.none[Int]

      l(-1) must throwA[IndexOutOfBoundsException]
      l(4) must throwA[IndexOutOfBoundsException]
    }
  }
}
