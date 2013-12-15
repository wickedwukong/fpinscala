package fpinscala.state

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck._

class RNGSpec extends Specification with ScalaCheck {

  "positiveRNG" should {
    "always generate positive integers" in {
      "different seeds" ! check {
        (seed: Long) => {
          RNG.positiveInt(RNG.simple(seed))._1 must be_>=(0)
        }
      }
    }
  }

  "double" should {
    "genereate a value between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          RNG.double(RNG.simple(seed))._1 must (be_>=(0.0) and be_<(1.0))
        }
      }
    }
  }

  "intDouble" should {
    "genereate an int value and a double value between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val value: ((Int, Double), RNG) = RNG.intDouble(RNG.simple(seed))
          value._1._2 must (be_>=(0.0) and be_<(1.0))
        }
      }
    }
  }

  "doubleInt" should {
    "genereate an int value and a double value between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val value: ((Double, Int), RNG) = RNG.doubleInt(RNG.simple(seed))
          value._1._1 must (be_>=(0.0) and be_<(1.0))
        }
      }
    }
  }

  "ints" should {
    "genereate 0 ints when the count is 0" in {
      "different seeds" ! check {
        (seed: Long) => {
          val (list, _) = RNG.ints(0)(RNG.simple(seed))
          list.length must_== (0)
        }
      }
    }

    "genereate 1 int when the count is 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val (list, _) = RNG.ints(1)(RNG.simple(seed))
          list.length must_==(1)
        }
      }
    }

    "genereate 2 ints when the count is 2 and they are not equal" in {
      "different seeds" ! check {
        (seed: Long) => {
          val (list, _) = RNG.ints(2)(RNG.simple(seed))
          list.length must_==(2)
          list(0) must_!=(list(1))
        }
      }
    }
  }

  "double3" should {
    "genereate a3 different double values between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val value: ((Double, Double, Double), RNG) = RNG.double3(RNG.simple(seed))
          value._1._1 must (be_>=(0.0) and be_<(1.0))
          value._1._2 must (be_>=(0.0) and be_<(1.0))
          value._1._3 must (be_>=(0.0) and be_<(1.0))

          value._1._1 must_!=(value._1._2)
          value._1._1 must_!=(value._1._3)
          value._1._2 must_!=(value._1._3)
        }
      }
    }
  }
}
