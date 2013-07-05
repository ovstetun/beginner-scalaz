package no.mesan

import scala.language.implicitConversions

trait Truthy[A] {
  def truthy(a: A): Boolean
}

object Truthy {
  def apply[A](implicit A: Truthy[A]): Truthy[A] = A
  def truthy[A](f: A => Boolean) = new Truthy[A]{
    def truthy(a: A): Boolean = f(a)
  }

  implicit val intTruthy: Truthy[Int] = new Truthy[Int] {
    def truthy(a: Int): Boolean = a match {
      case 0 => false
      case _ => true
    }
  }
  implicit def listTrutry[A]: Truthy[List[A]] = new Truthy[List[A]] {
    def truthy(a: List[A]): Boolean = a match {
      case Nil => false
      case _ :: _ => true
    }
  }
  implicit val nilTruthy: Truthy[Nil.type] = Truthy.truthy(_ => false)
}

trait TruthyOps[A] {
  def self: A
  implicit def F:Truthy[A]
  def truthy: Boolean = F.truthy(self)
}

object ToTruthyOps {
  implicit def toTruthyOps[A](a: A)(implicit ev: Truthy[A]) = new TruthyOps[A] {
    def self: A = a
    implicit def F = ev
  }

  def iffy[A: Truthy, B](t: A)(ifTrue: => B)(ifFalse: => B): B = {
    if (t.truthy) ifTrue else ifFalse
  }
}
