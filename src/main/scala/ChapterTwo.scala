package kuba.sicp

import scala.language.implicitConversions

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

object ChapterTwo {
  
}