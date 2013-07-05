package no.mesan

import org.specs2.mutable.Specification

import scalaz._

class TagSpec extends Specification {

  sealed trait Kilogram
  def Kilogram[A](a: A) : A @@ Kilogram = Tag[A, Kilogram](a)

  sealed trait KilogramD
  def KilogramD(a: Int) : Int @@ Kilogram = Tag[Int, Kilogram](a)

  sealed trait JoulePerKiloGram
  def JoulePerKiloGram[A](a: A): A @@ JoulePerKiloGram = Tag[A, JoulePerKiloGram](a)

  "Tag" should {
    "pend" in {
      def energyD(m: Double @@ Kilogram): Double @@ JoulePerKiloGram = JoulePerKiloGram(100.0 * m) //(299792458.0 * 299792458.0 * m)
      def energyI(m: Int @@ Kilogram): Int @@ JoulePerKiloGram = JoulePerKiloGram (200 * m) //(299792458 * 299792458 * m)

      val massD: Double @@ Kilogram = Kilogram(78.0)
      massD * 2.0 must_== Kilogram(156.0)
      massD * 2.0 must_== 156.0

      val massI: Int @@ Kilogram = Kilogram(78)
      massI * 2 must_== Kilogram(156)
      massI * 2 must_== 156

      energyD(massD) must_== Kilogram(7800.0)
      energyI(massI) must_== Kilogram(15600)
    }
    "" in {
      sealed trait Age
      def Age(a: Int) = Tag[Int, Age](a)

      def doubleAge(a: Int @@ Age): Int @@ Age = Age(a*2)

      doubleAge(Age(4)) must_== Age(8)
//      doubleAge(4) //doesn't compile, not tagged as Age :)
    }
  }
}
