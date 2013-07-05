package no.mesan

import scalaz._, Scalaz._
import org.specs2.mutable.Specification

case class Point(x:Double, y:Double)
case class Color(r: Byte, g: Byte, b: Byte)

case class Turtle(position: Point, heading: Double, color: Color) {
  def forward(dist: Double): Turtle =
    copy(position =
            position.copy(
              x = position.x + dist * math.cos(heading),
              y = position.y + dist * math.sin(heading)
            ))
}

class LensSpec extends Specification {
  val turtle = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

  "Turtle" should {
    "move forward" in {
      turtle.forward(10) must_== Turtle(Point(12.0, 3.0), 0.0, Color(-1, -1, -1))
    }
  }
}
