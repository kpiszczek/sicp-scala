package kuba.sicp

// ex 2.7, 2.9
case class Interval(lower: Double, upper: Double) {
  def +(that: Interval) =
    Interval(lower + that.lower, upper + that.upper)
  def -(that: Interval) =
    Interval(lower - that.lower, upper - that.upper)
  def *(that: Interval) = {
    val p1 = lower * that.lower
    val p2 = lower * that.upper
    val p3 = upper * that.lower
    val p4 = upper * that.upper
    val ps = List(p1, p2, p3, p4)
    Interval(ps.min, ps.max)
  }
  def /(that: Interval) = 
    if (that.lower == 0.0 || that.upper == 0.0) 
      throw new Exception("Undefined interval")
    else 
      this * Interval(1.0 / that.lower, 1.0 / that.upper)
  lazy val width = (upper - lower) / 2.0
  lazy val center = (upper + lower) / 2.0
  lazy val percent = (width / center) * 100.0
  def asCenter = (center, percent)
  def printCenter = s"[${center}, ${percent}]"
}

object Interval {
  def fromCenter(center: Double, percent: Double) = {
    val width = center * (percent / 100.0)
    Interval(center - width, center + width)
  } 
}