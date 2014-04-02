package fpinscala.monoids

import org.specs2.mutable.Specification
import fpinscala.monoids.Monoid._

class MonoidSpec extends Specification {
  "intAddition" should {
    "follow monoid associative laws" in {
      intAddition.op(intAddition.op(1, 2), 3) must_== intAddition.op(1, intAddition.op(2, 3))
    }

    "zero law" in {
      intAddition.op(intAddition.zero, 100) must_== 100
      intAddition.op(100, intAddition.zero) must_== 100
    }
  }

  "optionMonoid" should {
    "follow monoid associative laws" in {
      optionMonoid.op(optionMonoid.op(Some(1), Some(2)), Some(3)) must_== optionMonoid.op(Some(1), optionMonoid.op(Some(2), Some(3)))
    }

    "zero law" in {
      optionMonoid.op(optionMonoid.zero, Some(100)) must_== Some(100)
      optionMonoid.op(Some(100), optionMonoid.zero) must_== Some(100)
    }
  }

  "endoMonoid" should {
    "follow monoid associative laws" in {
      endoMonoid.op(endoMonoid.op((a: Int) => 1, (b: Int) => 2), (c: Int) => 3)(-999) must_== endoMonoid.op((a: Int) => 1, endoMonoid.op((b: Int) => 2, (c: Int) => 3))(-999)
    }

    "zero law" in {
      endoMonoid.op(endoMonoid.zero, (a: String) => "hello")("whatever") must_== "hello"
      endoMonoid.op((a: String) => "hello", endoMonoid.zero)("whatever") must_== "hello"
    }
  }
}
