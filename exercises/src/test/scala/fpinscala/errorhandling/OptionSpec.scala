package fpinscala.errorhandling

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "map" should {
    "transform None to None" in {
      None.map(a => 1) must_== None
    }
  }
}
