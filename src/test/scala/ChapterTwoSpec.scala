package kuba.sicp

import org.specs2.mutable._

class ChapterTwoSpec extends Specification {
  import ChapterTwo._
  import Symbols._

  "deriv" should {
    "of x + 3 for x should be 1 0" in {
      deriv(Sum(Variable("x"), Number(3)), Variable("x")) must equalTo(Sum(Number(1), Number(0)))
    }
    "of x * y for x should be 0* x + 1 * y" in {
      deriv(Product(Variable("x"), Variable("y")), Variable("x")) must equalTo (
        Sum(
          Product(Number(0), Variable("x")),
          Product(Number(1), Variable("y"))))
    }
  }
}