package no.mesan

import org.specs2.mutable.Specification
import scalaz.Enum
import scalaz.Ordering
import scalaz.Digit
import scalaz.syntax.enum._
import scalaz.std.AllInstances.intInstance
import scalaz.std.AllInstances.char

class EnumSpec extends  Specification {

  "Enum" should {
    "define next" in {
      1.succ must_== 2
      1.succx must beSome(2)
    }
    "define previous" in {
      2.pred must_== 1
      2.predx must beSome(1)
    }
    "produce a range" in {
      1 |-> 4 must_== (1 to 4)
      1 |-> 4 must_== List(1,2,3,4)

      'a' |-> 'f' must_== List('a','b','c','d','e','f')

      1 |--> (2, 10) must_== List(1,3,5,7,9)
      1 |--> (2, 10) must_== (1 to 10 by 2)

      1.from.take(5) must_== List(1,2,3,4,5)
    }
    "be able to jump a range" in {
      1 -+- 4 must_== 5
      1 --- 4 must_== -3
    }
    "produce my own Enum" in {
      sealed trait Status
      case object New extends Status
      case object Open extends Status
      case object Closed extends Status

      implicit val statusEnum = new Enum[Status] {
        override def max: Option[Status] = Some(Closed)
        override def min: Option[Status] = Some(New)

        def succ(a: Status): Status = a match {
          case New => Open
          case Open => Closed
          case Closed => Closed
        }
        def pred(a: Status): Status = a match {
          case Open => New
          case Closed => Open
          case New => New
        }
        import Ordering._
        def order(x: Status, y: Status): Ordering = (x, y) match {
          case (x, y) if x == y => EQ
          case (New, _) => LT
          case (Closed, _) => GT
          case (_, Closed) => LT
          case _ => LT
        }
      }

      (New: Status).succ must_== Open
      (New: Status).pred must_== New
      (New:Status) |-> (Closed:Status) must_== List(New, Open, Closed)
      (New:Status) |-> (New:Status) must_== List(New)
    }
    "use digit as enum" in {
      (Digit._1: Digit).succ must_== Digit._2
      (Digit._1: Digit) |-> (Digit._4: Digit) must_== List(Digit._1, Digit._2, Digit._3, Digit._4)
    }
  }
}
