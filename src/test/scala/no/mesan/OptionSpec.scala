package no.mesan

import org.specs2.mutable.Specification

import scalaz.std.option
import scalaz.syntax.std.option._
import scalaz.syntax.std.OptionOps

//import scalaz.syntax.std.option
//import scalaz._, Scalaz._

class OptionSpec extends Specification {
  "Some" should {
    "be constructable" in {
      Some(42)
      42.some must_== Some(42)
      42.some must beAnInstanceOf[Option[Int]]
//      some(42) must_== Some(42) // doesn't work in specs, wrong 'some' is chosen
      option.some(42) must_== Some(42)
    }
  }
  "None" should {
    "be construcatble" in {
//    none[Int] must_== None // doesn't work in specs, wrong 'none' is chosen
      option.none[Int] must_== None
    }
  }
  "Option" should {
    "chain" in {
      val o = 42.some
      o some {_ + 1} none 2 must_== 43
      o.some(x => x+1).none(2) must_== 43

    }
    "getOrElse" in {
      val o = 42.some
      o getOrElse 1 must_== 42
      o | 1 must_== 42

      val n = option.none[Int]
      n getOrElse 1 must_== 1
      n | 1 must_== 1
    }
    "~" in {
      import scalaz.std.anyVal.intInstance

      val o = 42.some
      ~o must_== 42

      val n = option.none[Int]
      ~n must_== 0
    }
  }
  "cata" should {
    "doit" in {
      42.some.cata(_ => 1, 2) must_== 1
      (42.some: OptionOps[Int]).fold(_ => 1, 2) must_== 1 // needs explicit conversion to OptionOps for some weird reason...

      option.none[Int].cata(_ => 1, 2) must_== 2

      42.some.fold(2)(_ => 1) must_== 1 // ths is from the regular scala.Option class
      option.none[Int].fold(2)(_ =>1 ) must_== 2
    }
  }
}
