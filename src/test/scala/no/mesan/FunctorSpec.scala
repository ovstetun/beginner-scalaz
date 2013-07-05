package no.mesan

import org.specs2.mutable.Specification

import scalaz.Functor
import scalaz.syntax.functor._

class FunctorSpec extends Specification {
  "TupleFunctor" should {
    import scalaz.std.tuple._
    "map to the last element" in {
      (1, 2, 3) map {_ + 1} must_== (1, 2, 4)
    }
  }
  "ListFunctor" should {
    import scalaz.std.list.listInstance
    "map all elements in list" in {
      List(1, 2, 3) map {_ + 1} must_== List(2,3,4)

      List(1,2,3) map {identity}

      val lf = implicitly[Functor[List]]
      lf.map(List(1,2,3)){_ + 1} must_== List(2,3,4)
    }
    ">| turns list into" in {
      List(1,2,3) >| "a" must_== List("a","a","a")
    }
    "fpair" in {
      (List(1,2,3) fpair) must_== List((1,1),(2,2),(3,3))
    }
    "fproduct" in {
      List(1,2,3) fproduct {_*2} must_== List((1,2), (2,4), (3,6))
    }
  }
}
