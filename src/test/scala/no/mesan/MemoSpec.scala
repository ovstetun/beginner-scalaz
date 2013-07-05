package no.mesan

import scalaz._, Scalaz._
import org.specs2.mutable.Specification

class MemoSpec extends Specification {
  val slowFib: Int => Int = {
    case 0 => 0
    case 1 => 1
    case x => slowFib(x-2) + slowFib(x-1)
  }

  val memoFib: Int => Int = Memo.immutableHashMapMemo{
    case 0 => 0
    case 1 => 1
    case x => memoFib(x-2) + memoFib(x-1)
  }

  "Memo" should {
    "act like a function" in {
      slowFib(10) must_== 55
      slowFib(30) must_== 832040
//      slowFib(45) must_== 1134903170 // takes 7secs

      memoFib(10) must_== 55
      memoFib(30) must_== 832040
      memoFib(45) must_== 1134903170
    }
  }
}
