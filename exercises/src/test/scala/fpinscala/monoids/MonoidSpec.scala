package fpinscala.monoids

import org.specs2.mutable.Specification
import Monoid._

class MonoidSpec extends Specification {
  "intAddition" should {
    "follow monoid associative laws" in {
      intAddition.op(1, 2) must_== intAddition.op(2, 1)
    }
    "zero law" in {
      intAddition.op(intAddition.zero, 100) must_== 100
      intAddition.op(100, intAddition.zero) must_== 100
    }
  }
}
