package kuba.sicp

import scala.language.implicitConversions
import scala.math.{sqrt, pow, log}

class Rational(n: Int, d: Int) {
  import ChapterOne.gcd
  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  private val sign = n.signum * d.signum
  val numer = sign * n.abs / g
  val denom = d.abs / g

  override def toString = s"$numer / $denom"

  def +(that: Rational) = new Rational(
    (numer * that.denom) + (that.numer * denom),
    denom * that.denom)

  def -(that: Rational) = new Rational(
    (numer * that.denom) - (that.numer * denom),
    denom * that.denom)

  def *(that: Rational) = new Rational(
    numer * that.numer,
    denom * that.denom)

  def /(that: Rational) = new Rational(
    numer * that.denom,
    denom * that.numer)

  override def equals(that: Any) = that match {
    case Rational(n, d) => numer * d == n * denom
    case _ => false
  }
}

object Rational {
  implicit def intToRational(x: Int) = Rational(x)
  def apply(n: Int, d: Int) = new Rational(n, d)
  def apply(n: Int) = new Rational(n, 1)
  def unapply(rational: Rational): Option[(Int, Int)] = 
    Some((rational.numer, rational.denom))
}

// ex 2.2
case class Point(x: Double, y: Double)
case class Segment(start: Point, end: Point) {
  lazy val midpoint = Point((start.x + end.x) / 2.0, (start.y + end.y) / 2.0)
}
// ex.2.3
case class Rectangle(val a: Point, val b: Point) {
  import ChapterOne.abs
  lazy val area = ((b.x - a.x) * (b.y - a.y)).abs
  lazy val perimeter = abs(2.0 * (b.x - a.x)) + abs(2.0 * (b.y - a.y))
}
object Rectangle {
  def apply(topLeft: Point, width: Double, height: Double): Rectangle = 
    Rectangle(topLeft, Point(topLeft.x + width, topLeft.y - height))
}

// ex 2.5
class Pair(value: Double) {
  def car: Double = 
    if (value % 3 == 0) (new Pair(value / 3)).car
    else log(value) / log(2)
  def cdr: Double = 
    if (value % 2 == 0) (new Pair(value / 2)).cdr
    else log(value) / log(3)
}

object Pair {
  def apply(a: Double, b: Double) =
    new Pair(pow(2, a) * pow(3, b))
}


object ChapterTwo {
  def cons[A](x: A, y: List[A]) = x :: y
  def car[A](x: List[A]): Option[A] = x match {
    case Nil => None
    case a :: _ => Some(a)
  }
  def cdr[A](x: List[A]): Option[List[A]] = x match {
    case Nil | _ :: Nil => None
    case _ :: tail => Some(tail)
  }
}