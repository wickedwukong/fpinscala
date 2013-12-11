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
}
