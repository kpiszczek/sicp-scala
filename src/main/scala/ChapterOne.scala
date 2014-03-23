package kuba.sicp

import Numeric._
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}
import scala.math.{log, pow}

object ChapterOne {
  val Random = scala.util.Random

  def square[A](x: A)(implicit num: Numeric[A]) =
    num.times(x, x)

  def cube[A](x: A)(implicit num: Numeric[A]) =
    num.times(x, square(x))

  def sumOfSquares[A](x: A, y: A)(implicit num: Numeric[A]) =
    num.plus(square(x), square(y))

  def abs[A](x: A)(implicit num: Numeric[A]) = {
    if (num.lt(x, num.zero)) num.negate(x)
    else x
  }

  def isEven(n: Int) = n % 2 == 0
  def isOdd(n: Int) = n % 2 != 0

  def average[A](x: A, y: A)(implicit num: Numeric[A]) =
    num.toDouble(num.plus(x, y)) / 2.0

  def factorial(n: Int) = {
    @tailrec def factIter(product: Int, counter: Int): Int = {
      if (counter > n) product
      else factIter(counter * product, counter + 1)
    }
    factIter(1, 1)
  }

  def countChange(amount: Int) = {
    val kindsOfCoins = 1 :: 5 :: 10 :: 25 :: 50 :: Nil
    def cc(amount: Int, kindsOfCoins: List[Int]): Int = {
      (amount, kindsOfCoins) match {
        case (0, _) => 1
        case (_, Nil) => 0
        case (amount, _) if amount < 0 => 0
        case (amount, coinValue :: remainingTypes) => {
          cc(amount, remainingTypes) + cc(amount - coinValue, kindsOfCoins)
        }
      }
    }
    cc(amount, kindsOfCoins)
  }

  def inc[A](x: A)(implicit num: Numeric[A]) = num.plus(x, num.one)
  def dec(x: Int) = x - 1

  def fibonacci(n: Int): Int = {
    @tailrec def fibIter(a: Int, b: Int, count: Int): Int = {
      if (count == 0) b
      else fibIter(a + b, a, count - 1)
    }
    fibIter(1, 0, n)
  }

  def expt[A](b: A, n: Int)(implicit num: Numeric[A]) = {
    @tailrec def exptIter(counter: Int, product: A): A = {
      if (counter == 0) product
      else exptIter(counter - 1, num.times(b, product))
    }
    exptIter(n, num.one)
  }

  @tailrec def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def divides(a: Int, b: Int) = b % a == 0

  @tailrec def findDivisor(n: Int, testDivisor: Int): Int = {
    if (square(testDivisor) > n) n
    else if (divides(testDivisor, n)) testDivisor
    else findDivisor(n, testDivisor + 1)
  }

  def smallestDivisor(n: Int) = findDivisor(n, 2)

  def isPrime(n: Int) = smallestDivisor(n) == n

  def expmod(base: Int, exp: Int, m: Int): Int = {
    if (exp == 0) 1
    else if (isEven(exp)) square(expmod(base, exp / 2, m)) % m
    else (base * expmod(base, exp - 1, m)) % m
  }

  def fermatTest(n: Int) = {
    def tryIt(a: Int) = expmod(a, n, n) == a
    tryIt(1 + Random.nextInt(n - 1))
  }

  @tailrec def isPrimeFast(n: Int, times: Int): Boolean = {
    if (times == 0) true
    else if (fermatTest(n)) (isPrimeFast(n, times - 1))
    else false
  }

  // ex 1.3
  def greaterTwoSumOfSquares[A](x: A, y: A, z: A)(implicit num: Numeric[A]) = {
    if (num.equiv(x, if (num.gt(x, y)) x else y))
      sumOfSquares(x, if (num.gt(y, z)) y else z)
    else
      sumOfSquares(y, if (num.gt(x, z)) x else z)
  }

  // ex 1.7
  def sqrt(x: Double) = {
    def goodEnough(guess: Double) = abs(square(guess) - x) < 0.001 * x

    def improve(guess: Double) = average(guess, x / guess)

    @tailrec def sqrtIter(guess: Double): Double = {
      if (goodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    sqrtIter(1.0)
  }

  // ex 1.8
  def cubeRoot(x: Double) = {
    def goodEnough(guess: Double) = abs(cube(guess) - x) < 0.0001 * x

    def improve(guess: Double) = (x / square(guess) + 2 * guess) / 3.0

    @tailrec def cubeRootIter(guess: Double): Double = {
      if (goodEnough(guess)) guess
      else cubeRootIter(improve(guess))
    }

    cubeRootIter(1.0)
  }

  // ex 1.9
  def addRec(a: Int, b: Int): Int = {
    if (a == 0) b
    else inc(addRec(dec(a), b))
  }

  @tailrec def addIter(a: Int, b: Int): Int = {
    if (a == 0) b
    else addIter(dec(a), inc(b))
  }

  // ex 1.10
  def ackermann(x: Int, y: Int): Int = {
    if (y == 0) 0
    else if (x == 0) 2 * y
    else if (y == 1) 2
    else ackermann(x - 1, ackermann(x, y - 1))
  }

  object ex1_10 {
    // 2 * n
    def f(n: Int) = ackermann(0, n)
    // 2^n
    def g(n: Int) = ackermann(1, n)
    // 2^2^...n-times
    def h(n: Int) = ackermann(2, n)
    def k(n: Int) = 5 * n * n
  }

  // ex 1.11
  def ex1_11Rec(n: Int): Int = {
    if (n < 3) n
    else ex1_11Rec(n - 1) + 2 * ex1_11Rec(n - 2) + 3 * ex1_11Rec(n - 3)
  }

  def ex1_11Iter(n: Int) = {
    @tailrec def inner(a: Int, b: Int, c: Int, count: Int): Int = {
      if (count == 0) a
      else inner(b, c, 3 * a + 2 * b + c, count - 1)
    }
    inner(0, 1, 2, n)
  }

  // ex 1.12
  def pascalTriang(row: Int, column: Int): Option[Int] = {
    (row, column) match {
      case (_, 0) => Some(1)
      case (r, c) if (c > r || r < 0 || c < 0) => None
      case (r, c) if (r == c) => Some(1)
      case (r, c) => (pascalTriang(r - 1, c - 1), pascalTriang(r - 1, c)) match {
        case (Some(v1), Some(v2)) => Some(v1 + v2)
        case _ => None
      }
    }
  }

  // ex 1.15
  def sine(angle: Double): Double = {
    val p = (x: Double) => 3 * x - 4 * cube(x)
    if (angle < 0.1) angle
    else p(sine(angle / 3.0))
  }

  // ex 1.16
  def fastExpt[A](b: A, n: Int)(implicit num: Numeric[A]) = {
    @tailrec def fastExptIter(a: A, b: A, n: Int): A = {
      if (n == 0) a
      else if (isEven(n)) fastExptIter(a, square(b), n / 2)
      else fastExptIter(num.times(a, b), b, n - 1)
    }
    fastExptIter(num.one, b, n)
  }

  // ex 1.17
  def multiplyByAddition[A](a: A, b: Int)(implicit num: Numeric[A]): A = {
    if (b == 0) num.zero
    else num.plus(a, multiplyByAddition(a, b - 1))
  }

  def halve[A](a: A)(implicit num: Numeric[A]) = num.fromInt(num.toInt(a) / 2)
  def double[A](a: A)(implicit num: Numeric[A]) = num.times(a, num.fromInt(2))

  // replaced with iterative process
  // def fastMultiplyByAddition[A](a: A, b: Int)(implicit num: Numeric[A]): A = {
  //   if (b == 0) num.zero
  //   else if (isEven(b)) double(fastMultiplyByAddition(a, halve(b)))
  //   else num.plus(a, fastMultiplyByAddition(a, b - 1))
  // }

  // ex 1.18
  def fastMultiplyByAddition[A](a: A, b: Int)(implicit num: Numeric[A]): A = {
    @tailrec def multIter(a: A, b: Int, acc: A): A = {
      if (b == 0) acc
      else if (isEven(b)) multIter(double(a), halve(b), acc)
      else multIter(a, b - 1, num.plus(acc, a))
    }
    multIter(a, b, num.zero)
  }

  // ex 1.19
  def fib(n: Int) = {
    @tailrec def fibIter(a: Int, b: Int, p: Int, q: Int, count: Int): Int = {
      if (count == 0)
        b
      else if (isEven(count))
        fibIter(a, b, square(p) + square(q), 2 * p * q + square(q), count / 2)
      else
        fibIter(b * q + a * q + a * p, b * p + a * q, p, q, count - 1)
    }
    fibIter(1, 0, 0, 1, n)
  }

  // ex 1.22 / 1.23
  def timedCall(fn: => Any) {
    val timeStart = System.nanoTime
    val result = fn
    val timeEnd = System.nanoTime
    println(s"Elapsed time: ${timeEnd - timeStart}")
  }

  def nextForTest(x: Int) = if (x % 2 == 0) x + 1 else x + 2 

  @tailrec
  def searchForPrimes(start: Int, end: Int): Option[Int] = 
    if (start > end) None
    else if (isPrime(start)) Some(start)
    else searchForPrimes(nextForTest(start), end)
    

  def findThreeSmallestPrimes(start: Int, end: Int): List[Int] = {
    @tailrec def go(start: Int, acc: List[Int]): List[Int] = 
      if (acc.length == 3 || start > end) acc
      else searchForPrimes(start, end) match {
        case None => acc
        case Some(prime) => go(nextForTest(prime), prime :: acc)
      }
    go(start, Nil)
  }

  // ex 1.28
  def millerRabinExpmod(base: Int, exp: Int, m: Int): Int = {
    def squareMod(x: Int): Int = {
      def checkNontrivialSqrt(sqr: Int): Int = 
        if (sqr == 1 && x != 1 && x != m - 1) 0
        else sqr
      checkNontrivialSqrt(square(x) % m)
    }
    exp match {
      case 0 => 1
      case exp if isEven(exp) => 
        squareMod(millerRabinExpmod(base, exp / 2, m))
      case exp => 
        (base * millerRabinExpmod(base, exp - 1, m)) % m
    }
  }

  def millerRabinTest(n: Int) = 
    millerRabinExpmod(
      Random.nextInt(n - 1) + 1,
      n - 1,
      n) == 1

  def millerFastPrime(n: Int, times: Int): Boolean = 
    if (times == 0) true
    else if (millerRabinTest(n)) millerFastPrime(n, times - 1)
    else false

  def sum[A](term: A => A, a: A, next: A => A, b: A)(implicit num: Numeric[A]): A = {
    @tailrec def go(a: A, acc: A): A = 
      if (num.gt(a, b)) acc
      else go(next(a), num.plus(term(a), acc))
    go(a, num.zero)
  }

  def sumCubes(a: Int, b: Int) = 
    sum[Int](cube, a, inc, b)

  def identity[A](a: A) = a

  def sumItegers(a: Int, b: Int) =
    sum[Int](identity[Int], a, inc, b)

  def piSum(a: Double, b: Double) = {
    def piTerm(x: Double) = 1.0 / (x * (x + 2.0))
    def piNext(x: Double) = x + 4.0
    sum[Double](piTerm, a, piNext, b)
  }

  def integral(f: Double => Double, a: Double, b: Double, dx: Double): Double = {
    def addDx(x: Double) = x + dx
    sum[Double](f, a + dx/2.0, addDx, b) * dx
  }

  // ex 1.29
  def simpsonIntegral(f: Double => Double, a: Double, b: Double, n: Int): Double = {
    val h = (b - a) / n
    def nthY(n: Double) = f(a + h*n)
    def term(k: Double) = nthY(k) * (if (k == 0 || n == k) 1.0
      else if (isOdd(k.toInt)) 4.0
      else 2.0)
    h / 3 * sum[Double](term, 0, inc, n)
  }

  // ex 1.31
  def product[A](term: A => A, a: A, next: A => A, b: A)(implicit num: Numeric[A]) = {
    @tailrec def go(a: A, acc: A): A =
      if (num.gt(a, b)) acc
      else go(next(a), num.times(term(a), acc))
    go(a, num.one)
  }

  val factorialUsingProduct: PartialFunction[Int, Int] = {
    case n: Int if n > 0 => product[Int](identity, 1, inc, n)
  }

  // ex 1.32
  def accumulate[A](combiner: (A, A) => A, nullValue: A, term: A => A, a: A, next: A => A, b: A)
    (implicit num: Numeric[A]) = {
    @tailrec def go(a: A, acc: A): A = 
      if (num.gt(a, b)) acc
      else go(next(a), combiner(acc, term(a)))
    go(a, nullValue)
  }

  val factorialUsingAccumulate: PartialFunction[Int, Int] = {
    case n: Int if n > 0 => accumulate[Int](_ * _, 1, identity, 1, inc, n)
  }

  // ex 1.33
  def filteredAccumulate[A](
    combiner: (A, A) => A, nullValue: A, term: A => A, a: A, next: A => A, b: A, pred: A => Boolean)
  (implicit num: Numeric[A]) = {
    @tailrec def go(a: A, acc: A): A = 
      if (num.gt(a, b)) acc
      else if (pred(a)) go(next(a), combiner(acc, term(a)))
      else go(next(a), acc)
    go(a, nullValue)
  }

  // ex 1.33a
  def sumSqurePrimes(a: Int, b: Int): Int = 
    filteredAccumulate[Int](
      _ + _, 0, square, a, inc, b, millerFastPrime(_, 10))

  // ex 1.33b
  def productCoprimes(n: Int): Int =
    filteredAccumulate[Int](
      _ * _, 1, identity, 1, inc, n - 1, i => gcd(i, n) == 1)

  def closeEnough(x: Double, y: Double) = abs[Double](x - y) < 0.001

  def search(f: Double => Double, negPoint: Double, posPoint: Double): Double = {
    val midPoint = average[Double](negPoint, posPoint)
    if (closeEnough(negPoint, posPoint)) midPoint
    else f(midPoint) match {
      case x if x > 0.0 => search(f, negPoint, midPoint)
      case x if x < 0.0 => search(f, midPoint, posPoint)
      case _ => midPoint
    }
  }

  def halfIntervalMethod(f: Double => Double, a: Double, b: Double): Try[Double] = {
    val aValue = f(a)
    val bValue = f(b)
    if (aValue < 0.0 && bValue > 0.0) Success(search(f, a, b))
    else if (bValue < 0.0 && aValue > 0.0) Success(search(f, b, a))
    else Failure(new Exception(s"Values are not of opposite sign: $a $b"))
  }

  val tolerance = 0.00001

  def fixedPoint(f: Double => Double, firstGuess: Double): Double = {
    def closeEnough(v1: Double, v2: Double) = abs(v1 - v2) < tolerance
    @tailrec def tryGuess(guess: Double): Double = {
      val next = f(guess)
      if (closeEnough(guess, next)) next
      else tryGuess(next)
    }
    tryGuess(firstGuess)
  }

  def sqrt2(x: Double) = fixedPoint(y => average(y, x / y), 1.0)

  // ex 1.37
  def contFrac(n: Int => Double, d: Int => Double, k: Int): Double = {
    @tailrec def go(i: Int, acc: Double): Double = 
      if (i == 0) acc
      else go(i - 1, n(i) / (d(i) + acc))
    go(k, 0.0)
  }

  // ex 1.38
  def euler(k: Int): Double = 
    2.0 + contFrac(
      _ => 1.0, 
      i => if (i % 3.0 == 2.0) 2.0/3.0*i + 2.0/3.0 else 1.0,
      k)

  // ex 1.39
  def tanCf(x: Double, k: Int) = 
    contFrac(
      i => if (i == 1) x else -square[Double](x),
      i => i * 2.0 - 1.0,
      k)

  def averageDamp(f: Double => Double) = 
    (x: Double) => average[Double](x, f(x))

  def deriv(g: Double => Double) = {
    val dx = 0.00001
    (x: Double) => (g(x + dx) - g(x)) / dx
  }

  def newtonTransform(g: Double => Double) =
    (x: Double) => g(x) / deriv(g)(x) - x 

  def newtonsMethod(g: Double => Double, guess: Double) = 
    fixedPoint(newtonTransform(g), guess)

  def fixedPointOfTransform(
    g: Double => Double, transform: (Double => Double) => (Double => Double), guess: Double) = 
    fixedPoint(transform(g), guess)

  // ex 1.40
  def cubic(a: Double, b: Double, c: Double) = 
    (x: Double) => cube(x) + a*square(x) + b*x + c

  // ex 1.41
  def double[A](f: A => A) =
    (x: A) => f(f(x))

  // ex 1.42
  def compose[A, B, C](f: B => C, g: A => B): A => C = 
    (x: A) => f(g(x))

  // ex 1.43
  def repeated[A](f: A => A, n: Int): A => A = 
    if (n < 1) identity[A] _
    else compose(f, repeated(f, n - 1))

  // ex 1.44
  def smooth(f: Double => Double) = {
    val dx = 0.00001
    (x: Double) => (f(x - dx) + f(x) + f(x + dx)) / 3
  }

  def nFoldSmooth(f: Double => Double, n: Int) = 
    repeated(smooth, n)(f)

  // ex 1.45
  def nthRoot(x: Double, n: Int) =
    fixedPoint(
      repeated(averageDamp, (log(x) / log(2)).toInt)(
        y => x / pow(y, n - 1)),
      1.0)

  // ex 1.46
  def iterativeImprove[A](improve: A => A, closeEnough: (A, A) => Boolean): A => A =
    (x: A) => {
      val nextX = improve(x)
      if (closeEnough(x, nextX)) nextX
      else iterativeImprove(improve, closeEnough)(nextX)
    }

  def sqrtUsingIterImprove(x: Double) = 
    iterativeImprove[Double](y => (x/y + y)/2.0, closeEnough)(1.0)

  def fixedPointUsingIterImprove(f: Double => Double, firstGuess: Double) =
    iterativeImprove(f, closeEnough)(firstGuess)
}