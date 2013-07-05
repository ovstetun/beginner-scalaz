package no.mesan

import org.specs2.mutable.Specification

import scalaz._, Scalaz._
import scalaz.std.{option => o}

case class Prob[A](list: List[(A, Double)])

trait ProbInstances {
  def flatten[B](xs: Prob[Prob[B]]): Prob[B] = {
    def multall(innerxs: Prob[B], p: Double) =
      innerxs.list map {
        case (x, r) => (x, p * r)
      }
    Prob((xs.list map {
      case (innerxs, p) => multall(innerxs, p)
    }).flatten)
  }
  implicit val probInstance = new Functor[Prob] with Monad[Prob] {
    def point[A](a: => A): Prob[A] = Prob((a, 1.0) :: Nil)
    def bind[A, B](fa: Prob[A])(f: (A) => Prob[B]): Prob[B] = flatten(map(fa)(f))

    override def map[A, B](fa: Prob[A])(f: A => B): Prob[B] =
      Prob(fa.list map {case (x, p) => (f(x), p)})
  }

  implicit def probShow[A]: Show[Prob[A]] = Show.showA
}

case object Prob extends ProbInstances


class MonadSpec extends Specification{
  "Monad" should {
    "bind is flatmap" in {
      val f: Int => Option[Int] = (x:Int) => (x * 10).some

      Bind[Option].bind(2.some){x => (x*3).some} must_== 6.some
      (9.some >>=     {x => Monad[Option].point[Int](x * 10)}) must_== 90.some
      (9.some >>=     f ) must_== 90.some
      (9.some >>=     {x => (x * 10).some}) must_== 90.some
      (9.some flatMap {x => Monad[Option].point(x * 10)}) must_== 90.some

      (9.some >> 4.some) must_== 4.some

      (9.some flatMap f) must_== 90.some
      (for {
        x <- 9.some
        y <- f(x)
      } yield y) must_== 90.some
    }
  }
  "for notation" should {
    "" in {
      def justH: Option[Char] =
        for {
          (x :: xs) <- "hello".toList.some
        } yield x
      justH must_== 'h'.some
    }
  }
  "HighWalk" should {
    type Birds = Int
    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] =
        if (math.abs((left + n) - right) < 4) copy(left = left + n).some
        else o.none
      def landRight(n: Birds): Option[Pole] =
        if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
        else o.none
      def banana: Option[Pole] = o.none
    }
    val StartPole = Pole(0,0)
    "flatmap" in {
      StartPole.landLeft(1) must_== Pole(1,0).some
      StartPole.landLeft(1) flatMap {_.landLeft(2)} must_== Pole(3, 0).some
      StartPole.landLeft(10) flatMap {_.landRight(1)} must_== o.none
    }
    "using monadic startpoint" in {
      (Monad[Option].point(StartPole) >>= {_.landLeft(1)} >>= {_.landLeft(2)}) must_== Pole(3,0).some
      (Monad[Option].point(StartPole) >>= {_.landLeft(1)} >>= {_.landLeft(2)}) must_== Pole(3,0).some
    }
    "using for comprehenstion" in {
      val endPoint = for {
        start <- StartPole.some
        next  <- start.landLeft(1)
        end   <- next.landLeft(2)
      } yield end
      endPoint must_== Pole(3,0).some
    }
    "using a higherorder function" in {
      def left(n:Birds): Pole => Option[Pole] = (p:Pole) => p.landLeft(n)
      def right(n:Birds): Pole => Option[Pole] = (p:Pole) => p.landRight(n)
      val end2 = for {
        s <- StartPole.some
        n <- left(1)(s)
        e <- left(2)(n)
      } yield e
      end2 must_== Pole(3,0).some


      (StartPole.some >>= left(2) >>= left(1)) must_== Pole(3,0).some
      val e3 = StartPole.some >>= left(2) >>= {_.banana} >>= right(2)
      e3 must_== o.none[Pole]
    }
  }
  "Knight Position" should {
    case class KnightPos(c:Int, r:Int) {
      def move: List[KnightPos] =
        for {
          KnightPos(c2, r2) <- List(KnightPos(c + 2, r - 1), KnightPos(c + 2, r + 1),
            KnightPos(c - 2, r - 1), KnightPos(c - 2, r + 1),
            KnightPos(c + 1, r - 2), KnightPos(c + 1, r + 2),
            KnightPos(c - 1, r - 2), KnightPos(c - 1, r + 2)) if (
                ((1 |-> 8) contains c2) && ((1 |-> 8) contains r2))
        } yield KnightPos(c2, r2)

      def in3: List[KnightPos] =
       for {
         first <- move
         second <- first.move
         third <- second.move
       } yield third

      def canReach(pos: KnightPos) = in3 contains pos
    }

    "produce correct moves" in {
      KnightPos(6, 2).move must_== List(KnightPos(8,1), KnightPos(8,3), KnightPos(4,1), KnightPos(4,3), KnightPos(7,4), KnightPos(5,4))
      KnightPos(8, 1).move must_== List(KnightPos(6,2), KnightPos(7,3))
    }
    "find places 3 moves away" in {
      KnightPos(6, 2).in3 must_== List(KnightPos(8,1), KnightPos(8,3), KnightPos(4,1), KnightPos(4,3), KnightPos(7, 4), KnightPos(5, 4), KnightPos(5, 2), KnightPos(5, 4), KnightPos(8, 1), KnightPos(8, 5), KnightPos(6, 1), KnightPos(6, 5), KnightPos(8, 1), KnightPos(8, 3), KnightPos(4, 1), KnightPos(4, 3), KnightPos(7, 4), KnightPos(5, 4), KnightPos(8, 3), KnightPos(8, 5), KnightPos(4, 3), KnightPos(4, 5), KnightPos(7, 2), KnightPos(7, 6), KnightPos(5, 2), KnightPos(5, 6), KnightPos(5, 2), KnightPos(8, 3), KnightPos(6, 3), KnightPos(5, 4), KnightPos(5, 6), KnightPos(8, 3), KnightPos(8, 7), KnightPos(6, 3), KnightPos(6, 7), KnightPos(8, 1), KnightPos(8, 3), KnightPos(4, 1), KnightPos(4, 3), KnightPos(7, 4), KnightPos(5, 4), KnightPos(4, 1), KnightPos(4, 3), KnightPos(3, 4), KnightPos(1, 4), KnightPos(7, 2), KnightPos(7, 4), KnightPos(3, 2), KnightPos(3, 4), KnightPos(6, 1), KnightPos(6, 5), KnightPos(4, 1), KnightPos(4, 5), KnightPos(5, 2), KnightPos(5, 4), KnightPos(1, 2), KnightPos(1, 4), KnightPos(4, 1), KnightPos(4, 5), KnightPos(2, 1), KnightPos(2, 5), KnightPos(8, 1), KnightPos(8, 3), KnightPos(4, 1), KnightPos(4, 3), KnightPos(7, 4), KnightPos(5, 4), KnightPos(8, 3), KnightPos(8, 5), KnightPos(4, 3), KnightPos(4, 5), KnightPos(7, 2), KnightPos(7, 6), KnightPos(5, 2), KnightPos(5, 6), KnightPos(4, 1), KnightPos(4, 3), KnightPos(3, 4), KnightPos(1, 4), KnightPos(4, 3), KnightPos(4, 5), KnightPos(3, 2), KnightPos(3, 6), KnightPos(1, 2), KnightPos(1, 6), KnightPos(7, 2), KnightPos(3, 2), KnightPos(6, 3), KnightPos(4, 3), KnightPos(7, 4), KnightPos(7, 6), KnightPos(3, 4), KnightPos(3, 6), KnightPos(6, 3), KnightPos(6, 7), KnightPos(4, 3), KnightPos(4, 7), KnightPos(5, 2), KnightPos(1, 2), KnightPos(4, 3), KnightPos(2, 3), KnightPos(5, 4), KnightPos(5, 6), KnightPos(1, 4), KnightPos(1, 6), KnightPos(4, 3), KnightPos(4, 7), KnightPos(2, 3), KnightPos(2, 7), KnightPos(7, 2), KnightPos(7, 4), KnightPos(3, 2), KnightPos(3, 4), KnightPos(6, 1), KnightPos(6, 5), KnightPos(4, 1), KnightPos(4, 5), KnightPos(7, 4), KnightPos(7, 6), KnightPos(3, 4), KnightPos(3, 6), KnightPos(6, 3), KnightPos(6, 7), KnightPos(4, 3), KnightPos(4, 7), KnightPos(6, 1), KnightPos(6, 3), KnightPos(7, 4), KnightPos(6, 5), KnightPos(6, 7), KnightPos(7, 4), KnightPos(7, 8), KnightPos(8, 1), KnightPos(8, 3), KnightPos(4, 1), KnightPos(4, 3), KnightPos(7, 4), KnightPos(5, 4), KnightPos(8, 5), KnightPos(8, 7), KnightPos(4, 5), KnightPos(4, 7), KnightPos(7, 4), KnightPos(7, 8), KnightPos(5, 4), KnightPos(5, 8), KnightPos(5, 2), KnightPos(5, 4), KnightPos(8, 1), KnightPos(8, 5), KnightPos(6, 1), KnightPos(6, 5), KnightPos(5, 4), KnightPos(5, 6), KnightPos(8, 3), KnightPos(8, 7), KnightPos(6, 3), KnightPos(6, 7), KnightPos(5, 2), KnightPos(5, 4), KnightPos(1, 2), KnightPos(1, 4), KnightPos(4, 1), KnightPos(4, 5), KnightPos(2, 1), KnightPos(2, 5), KnightPos(5, 4), KnightPos(5, 6), KnightPos(1, 4), KnightPos(1, 6), KnightPos(4, 3), KnightPos(4, 7), KnightPos(2, 3), KnightPos(2, 7), KnightPos(8, 1), KnightPos(8, 3), KnightPos(4, 1), KnightPos(4, 3), KnightPos(7, 4), KnightPos(5, 4), KnightPos(8, 5), KnightPos(8, 7), KnightPos(4, 5), KnightPos(4, 7), KnightPos(7, 4), KnightPos(7, 8), KnightPos(5, 4), KnightPos(5, 8), KnightPos(6, 1), KnightPos(6, 3), KnightPos(2, 1), KnightPos(2, 3), KnightPos(5, 4), KnightPos(3, 4), KnightPos(6, 5), KnightPos(6, 7), KnightPos(2, 5), KnightPos(2, 7), KnightPos(5, 4), KnightPos(5, 8), KnightPos(3, 4), KnightPos(3, 8))
    }
    "check if can reach point" in {
      (KnightPos(6,2) canReach KnightPos(6,1)) must beTrue
    }
  }
  "Prob" should {
    "lala" in {
      (Prob((3, 0.5) ::(5, 0.25) ::(9, 0.25) :: Nil) map { -_ }) must_== Prob((-3, 0.5) :: (-5, 0.25) :: (-9, 0.25) :: Nil)
    }
    "coins" in {
      sealed trait Coin
      case object Heads extends Coin
      case object Tails extends Coin
      implicit val coinEqual: Equal[Coin] = Equal.equalA

      def coin: Prob[Coin] = Prob(Heads -> 0.5 :: Tails -> 0.5 :: Nil)
      def loadedCoin: Prob[Coin] = Prob(List(Heads -> 0.1, Tails -> 0.9))

      def flipThree = for {
        a <- coin
        b <- coin
        c <- loadedCoin
      } yield {List(a,b,c) all {_ === Tails }}

      flipThree must_== Prob(List((false, 0.025), (false, 0.225), (false, 0.025), (false, 0.225), (false, 0.025), (false, 0.225), (false, 0.025), (true, 0.225)))
    }
  }
}

