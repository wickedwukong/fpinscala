package fpinscala.errorhandling

import org.specs2.mutable.Specification

class EitherSpec extends Specification {

  "map" should {
    "not transform Left" in {
      Left[String]("error").map(a => 1) must_== Left("error")
    }

    "transform Right" in {
      Right[Int](1).map(_ => "abc") must_== Right("abc")
    }
  }
}
