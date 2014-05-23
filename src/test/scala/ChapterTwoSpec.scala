package kuba.sicp

import org.specs2.mutable._

class ChapterTwoSpec extends Specification {
  import ChapterTwo._
  import Symbols._

  val x = Variable("x")

  "deriv" should {
    "of x + 3 for x should be 1" in {
      deriv(Sum(x, Number(3)), x) must equalTo(Number(1.0))
    }
    "of x * y for x should be y" in {
      deriv(Product(x, Variable("y")), x) must equalTo (Variable("y"))
    }
    "of x ^ 2 for x should by 2 * x" in {
      deriv(Exponentiation(x, Number(2)), x) must equalTo (Product(Number(2), x))
    }
  }
}