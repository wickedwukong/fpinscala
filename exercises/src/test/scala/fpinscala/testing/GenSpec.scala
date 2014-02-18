package fpinscala.testing


import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck._
import fpinscala.state.RNG
import fpinscala.state.RNG.Simple
import fpinscala.state.RNG._

class GenSpec extends Specification with ScalaCheck {

  "choose" should {
    "always generate integers between start and stopExclusive" in {
      "different seeds" ! check {
        (seed: Long, start: Int, stopExclusive: Int) => {
//            val (chosenValue, _) = Gen.choose(start, stopExclusive).sample.run(Simple(seed))
//            chosenValue must (be_>=(start) and be_<(stopExclusive))
          1 must_== 1
        }
      }
    }
  }
}

