package kuba.sicp

import org.specs2.mutable._

class ChapterOneSpec extends Specification {
  import ChapterOne._

  "Square function" should {
    "of 2 should be 4" in {
      square(2) must equalTo(4)
    }
    "of 1 should be 1" in {
      square(1) must equalTo(1)
    }
    "of 0 should be 0" in {
      square(0) must equalTo(0)
    }
    "of -2 should be 4" in {
      square(-2) must equalTo(4)
    }
  }

  "greaterTwoSumOfQuares" should {
    "be 13 for 1, 2, 3" in {
      greaterTwoSumOfSquares[Int](1, 2, 3) must equalTo(13)
    }
    "of -1, -1, -1 should be 2" in {
      greaterTwoSumOfSquares[Int](-1, -1, -1) must equalTo(2)
    }
    "of 3, 2, 1 should be 13" in {
      greaterTwoSumOfSquares[Int](3, 2, 1) must equalTo(13)
    }
    "of 3, 2, 3 should be 13" in {
      greaterTwoSumOfSquares[Int](3, 2, 3) must equalTo(18)
    }
  }

  "searchForPrimes" should {
    "compute smallest prime number in given range" in {
      searchForPrimes(1000, 10000) must beSome(1009)
    }
  }

  "findThreeSmallestPrimes" should {
    "compute smallest 3 primes in given range" in {
      findThreeSmallestPrimes(1000, 100000) must beLike {
        case List(1019, 1013, 1009) => true must_== true
      }
    }
  }

  "fermat test" should {
    "fail on Carmicheal numbers" in {
      isPrimeFast(561, 10) must beTrue
      isPrimeFast(1105, 10) must beTrue
      isPrimeFast(1729, 10) must beTrue
      isPrimeFast(2465, 10) must beTrue
      isPrimeFast(2821, 10) must beTrue
      isPrimeFast(6601, 10) must beTrue
    }
  }

  "miller-rabin" should {
    "not fail on Carmicheal numbers" in {
      millerFastPrime(561, 10) must beFalse
      millerFastPrime(1105, 10) must beFalse
      millerFastPrime(1729, 10) must beFalse
      millerFastPrime(2465, 10) must beFalse
      millerFastPrime(2821, 10) must beFalse
      millerFastPrime(6601, 10) must beFalse
    }
  }
} 
