package no.mesan

import org.specs2.mutable.Specification
import scalaz.{Apply, Applicative}
import scalaz.syntax.applicative._
import scalaz.std.{option => o}
import scalaz.syntax.std.option._

import scala.language.higherKinds

class ApplicativeSpec extends Specification {
  import scalaz.std.list._
  import scalaz.std.option._

  "Applicative List" should {
    "point to a list" in {
      1.point[List] must_== List(1)
      "asdf".point[List] must_== List("asdf")
      1.η[List] must_== List(1)
    }
    "apply to a list" in {
      val l = List(1,2,3)
      val f = (_:Int) * 3
      val f2 = (_:Int) * 2
      val fs = List(f, f2)

      l <*> f.point[List] must_== List(3,6,9)
      l <*> fs must_== List(3,6,9,2,4,6)

      l tuple l must_== List((1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3))

      (l |@| l) {_ + _} must_== List(2,3,4,3,4,5,4,5,6)
      (l |@| l).tupled must_== List((1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3))
    }
  }
  "Applicative Option" should {
    "point to an option" in {
      1.point[Option] must_== Some(1)
    }
    "apply on Some" in {
      val f = (_:Int) * 3
      2.some <*> f.some must_== 6.some
      9.some <*> { (_: Int) + 3 }.some must_== 12.some

      val f2 = (_: Int) * 2

      1.some <* 2.some must_== 1.some
      1.some <* f.some must_== 1.some
      1.some *> f.some must_== f.some

      1.some tuple 2.some must_== (1,2).some


      ^(1.some, 2.some) {_ + _} must_== 3.some // pulls out and applies function to values in tuple2
      ^^(1.some, 2.some, 3.some) {(x,y,z) => (x+y)*z} must_== 9.some // pulls out and applies function to values in tuple3

      (1.some |@| 2.some) {_ + _} must_== 3.some

      (f.some |@| f2.some).tupled must_== Some((f, f2))
      (f.some |@| f2.some) {(x: Int => Int, y: Int => Int) => 2} must_== 2.some

      def f3: (Int, Int) => Int = (x:Int, y:Int) => x + y

      f3(1,2) must_== 3
      f3(1.some, 2.some) must_== 3.some // automagisk lifting
      f3(1.some, o.none[Int]) must_== o.none[Int] // automagisk lifting

      val f3lifted: (Option[Int], Option[Int]) => Option[Int] = Apply[Option].lift2(f3)
      f3lifted(1.some, 2.some) must_== 3.some
      f3lifted(1.some, o.none[Int]) must_== o.none[Int]
    }
    "apply on None" in {
      val n:Option[Int] = o.none[Int]
      n <*> {(_:Int) * 3}.some must_== o.none[Int]
    }
  }
  "making sequenceA with applicatives" should {
    def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
      case Nil => (Nil: List[A]).point[F]
      case x :: xs => (x |@| sequenceA(xs)) {
        _ :: _
      }
    }
    "turn a list inside out" in {
      val l: List[Option[Int]] = List(1.some, 2.some, 3.some)
      val snudd: Option[List[Int]] = sequenceA(l)
      snudd must_== Some(List(1,2,3))

      val l2: List[Option[Int]] = List(1.some, o.none[Int], 3.some)
      sequenceA(l2) must_== o.none[List[Int]]
    }

  }
}
