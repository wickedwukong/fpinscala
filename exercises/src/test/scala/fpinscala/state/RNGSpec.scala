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
}
