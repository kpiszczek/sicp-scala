package kuba.sicp

import scala.annotation.tailrec
import scala.math.{sqrt, pow, log}

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

// class Church[A] {
//   type C[A] = (A => A) => (A => A)
//   var zero = (f: A => A) => (x: A) => x;
//   val one = add1(zero)
//   def add1(n: (A => A) => A => A)(f: A => A)(x: A) = f(n(f)(x));
//   def fromInt(n: Int) = {
//     @tailrec def go(n: Int, acc: C): C = 
//       if (n == 0) zero
//       else go(n - 1, add1(acc))
//     go(n, zero)
//   }
//   def toInt(church: C[A]): Int = 
//     church((n: Int) => n + 1)(0)
// }

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
  // ex 2.17
  @tailrec def last[A](as: List[A]): Option[A] = as match {
    case Nil => None
    case x :: Nil => Some(x)
    case _ :: tail => last(tail)
  }
  // ex 2.18
  def reverse[A](as: List[A]): List[A] = {
    @tailrec def go(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case head :: tail => go(tail, head :: acc)
    }
    go(as, Nil)
  }

  // ex 2.19
  val usCoins: List[Double] = List(50, 25, 10, 5, 1)
  val ukCoins: List[Double] = List(100, 50, 20, 10, 5, 2, 1, 0.5)

  def countChange(amount: Double, coinValues: List[Double]): Int = amount match {
    case 0.0 => 1
    case a if a < 0.0 || coinValues.isEmpty => 0
    case a => countChange(a, coinValues.tail) + countChange(a - coinValues.head, coinValues)
  }

  // ex 2.20
  def sameParity(values: Int*): Seq[Int] = {
    val parity = values.head % 2
    values.filter(_ % 2 == parity)
  }
}