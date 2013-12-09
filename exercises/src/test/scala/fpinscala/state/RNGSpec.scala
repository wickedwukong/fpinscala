package fpinscala.state

import org.specs2.mutable.Specification

class RNGSpec extends Specification {

  "positiveRNG" should {
    "always generate positive integers" in {
      1.until(100).map{_ =>
        RNG.positiveInt(RNG.simple(System.currentTimeMillis()))._1 must be_>=(0)
      }
    }
  }

}
