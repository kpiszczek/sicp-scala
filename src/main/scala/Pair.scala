package kuba.sicp

import scala.math.{sqrt, pow, log}

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