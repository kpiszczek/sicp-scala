package kuba.sicp.mobile

import scala.math

// ex 2.29
case class Mobile(val left: Branch, val right: Branch) {
  def totalWeight: Double = left.totalWeight + right.totalWeight
  def isBalanced: Boolean = {
    val totalLeft = left.totalLength * left.totalWeight
    val totalRight = right.totalLength * right.totalWeight
    val delta = math.min(totalLeft, totalRight)*0.001
    (totalRight - totalLeft).abs < delta
  }
}
case class Branch(length: Double, structure: Either[Double, Branch]) {
  require(length > 0)
  def totalWeight: Double = structure match {
    case Left(weight) => weight
    case Right(branch) => branch.totalWeight
  }
  def totalLength: Double = structure match {
    case Left(_) => length
    case Right(branch) => length + branch.totalLength
  }
}