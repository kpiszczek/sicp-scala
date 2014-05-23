package kuba.sicp

import annotation.tailrec
import math.{ sqrt, pow, log }
import util.{ Try, Success, Failure }

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

  // ex 2.32
  def subsets[A](as: List[A]): List[List[A]] = as match {
    case Nil => List(Nil)
    case a :: rest => {
      val ss = subsets(rest)
      ss ::: ss.map(x => a :: x)
    }
  }

  // ex 2.33
  def map[A, B](fn: A => B, as: List[A]): List[B] = 
    as.foldLeft(Nil: List[B])((acc, a) => acc :+ fn(a))
  def append[A](seq1: List[A], seq2: List[A]): List[A] =
    seq1.reverse.foldLeft(seq2)((acc, a) => a :: acc)
  def length[A](as: List[A]): Int =
    as.foldLeft(0)((acc, a) => acc + 1)

  // ex 2.34
  def hornerEval(x: Double, coeffSequence: List[Double]): Double =
    coeffSequence.foldRight(0.0)((coeff, acc) => coeff + acc*x)

  // ex 2.39
  def reverse2[A](as: List[A]) = 
    as.foldRight(Nil: List[A])((a, acc) => acc :+ a)
  def reverse3[A](as: List[A]) = 
    as.foldLeft(Nil: List[A])((acc, a) => a :: acc)

  // ex 2.40
  def uniquePairs(n: Int): Seq[(Int, Int)] =
    1 to n flatMap (i => 1 until i map (j => (i, j)))

  // ex 2.41
  def allOrderedTriples(n: Int, s: Int): Seq[(Int, Int, Int)] =
    1 to n flatMap (
      i => 1 until i flatMap (j => 
        1 until j map(k => (i, j, k)))
      ) filter (t => t._1 + t._2 + t._3 == s)

  // ex 2.42
  def queens(boardSize: Int) = {
    def inCheck(q1: (Int, Int), q2: (Int, Int)) =
      q1._1 == q2._1 || q1._2 == q2._2 || 
      (q1._1 - q2._1).abs == (q1._2 - q2._2).abs
    def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) = 
      queens forall (q => !inCheck(queen, q))
    def placeQueens(k: Int): List[List[(Int, Int)]] = 
      if (k == 0) List(Nil)
      else for {
        queens <- placeQueens(k - 1)
        col <- 0 until boardSize
        queen = (k-1, col)
        if isSafe(queen, queens)
      } yield queen :: queens
    placeQueens(boardSize)
  }
}

object Symbols {
  @tailrec
  def memq(item: Symbol, x: List[Symbol]): Boolean = x match {
    case Nil => false
    case a :: as => 
      if (a == item) true
      else memq(item, as)
  }

  // ex 2.56
  sealed abstract class Expression
  case class Variable(name: String) extends Expression
  case class Sum(addend: Expression, augend: Expression) extends Expression 
  case class Product(multiplier: Expression, multiplicand: Expression) extends Expression
  case class Number(value: Double) extends Expression
  case class Exponentiation(base: Expression, exponent: Number) extends Expression

  object Sum {
    def make(a1: Expression, a2: Expression): Expression = (a1, a2) match {
        case (Number(0.0), a2) => a2
        case (a1, Number(0.0)) => a1
        case (Number(a), Number(b)) => Number(a + b)
        case (a1, a2) => Sum(a1, a2)
      }
  }

  object Product {
    def make(m1: Expression, m2: Expression): Expression = (m1, m2) match {
      case (Number(0.0), _) | (_, Number(0.0)) => Number(0.0)
      case (a, Number(1.0)) => a
      case (Number(1.0), a) => a
      case (Number(n), Number(m)) => Number(n * m)
      case (m1, m2) => Product(m1, m2)
    }
  }

  object Exponentiation {
    def make(b: Expression, e: Number): Expression = (b, e) match {
      case (_, Number(0.0)) => Number(0.0)
      case (a, Number(1.0)) => a
      case (b, e) => Exponentiation(b, e)
    }
  }

  def deriv(expression: Expression, variable: Variable): Expression = 
    expression match {
      case Number(_) => Number(0)
      case v: Variable => Number(
          if (v == variable) 1
          else 0
        )
      case Sum(a1, a2) => 
        Sum.make(deriv(a1, variable), deriv(a2, variable))
      case Product(m1, m2) => Sum.make(
          Product.make(deriv(m2, variable), m1),
          Product.make(deriv(m1, variable), m2)
        )
      case Exponentiation(b, e) => 
        Product.make(e, Exponentiation.make(b, Number(e.value - 1.0)))

      case exp => throw new Exception(s"Unknown expression type: ${exp}")
    }
}