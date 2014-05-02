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

  "trimMonoid" should {
    "insert space between words" in {
      trimMonoid.op("abc", "cde") must_== "abc cde"
    }

    "does not insert space between two words if there are already one" in {
      trimMonoid.op("abc", " cde") must_== "abc cde"
    }

    "trim spaces" in {
      trimMonoid.op(" abc", " cde ") must_== "abc cde"
    }

    "follow monoid associative laws" in {
      trimMonoid.op("abc", trimMonoid.op("cde", "efg")) must_== trimMonoid.op(trimMonoid.op("abc", "cde"), "efg")
    }

    "zero law" in {
      trimMonoid.op("abc", trimMonoid.zero) must_== "abc"
      trimMonoid.op(trimMonoid.zero, "abc") must_== "abc"
    }
  }

  "order" should {
    "give true for a seq in asc order" in {
      ordered(IndexedSeq()) must_== true
      ordered(IndexedSeq(1)) must_== true
      ordered(IndexedSeq(1, 2)) must_== true
      ordered(IndexedSeq(1, 2, 3)) must_== true
    }

    "give false for a seq in desc order" in {
      ordered(IndexedSeq(2, 1)) must_== false
      ordered(IndexedSeq(3, 2, 1)) must_== false
      ordered(IndexedSeq(1, 2, 4, 3)) must_== false
      ordered(IndexedSeq(1, 3, 2, 3)) must_== false
      ordered(IndexedSeq(-1, 1, -1)) must_== false
    }
  }

  "order2" should {
    "give true for a seq in asc order" in {
      ordered(IndexedSeq()) must_== true
      ordered2(IndexedSeq(1)) must_== true
      ordered2(IndexedSeq(1, 2)) must_== true
      ordered2(IndexedSeq(1, 2, 3)) must_== true
    }

    "give false for a seq in desc order" in {
      ordered2(IndexedSeq(2, 1)) must_== false
      ordered2(IndexedSeq(3, 2, 1)) must_== false
      ordered2(IndexedSeq(1, 2, 4, 3)) must_== false
      ordered2(IndexedSeq(1, 3, 2, 3)) must_== false
      ordered2(IndexedSeq(-1, 1, -1)) must_== false
    }
  }

  "count" should {
    "count words" in {
      count("") must_== 0
      count("a") must_== 1
      count(" a") must_== 1
      count("a ") must_== 1
      count(" a ") must_== 1
      count("a b") must_== 2
      count(" a b") must_== 2
      count("a b ") must_== 2
      count(" a b ") must_== 2
      count(" a bc") must_== 2
      count("ab c") must_== 2
      count("ab cd") must_== 2
      count("ab cd ") must_== 2
      count("ab   cd ") must_== 2
      count("ab cd ef") must_== 3
      count("ab cd ef gh") must_== 4
      count("ab cd ef gh ij") must_== 5
      count("ab cd ef gh ij ") must_== 5
      count(" ab cd ef gh ij ") must_== 5
      count(" ab cd    ef gh ij ") must_== 5
    }
  }
}
