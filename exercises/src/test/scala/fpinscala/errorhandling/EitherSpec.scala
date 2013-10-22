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

  "flatMap" should {
    "not transform Left" in {
      Left[String]("error").flatMap(a => Right(1)) must_== Left("error")
    }

    "transform Right" in {
      Right[Int](1).flatMap(a => Right("abc")) must_== Right("abc")
    }
  }
}
