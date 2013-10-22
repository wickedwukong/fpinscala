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

  "orElse" should {
    "give left for two left" in {
      Left("error-1") orElse Left("error-2") must_== Left("error-2")
    }

    "give right for a left and a right" in {
      Left("error") orElse Right(1) must_== Right(1)
      Right(1) orElse Left("error")  must_== Right(1)
      Right(1) orElse Right(2)  must_== Right(1)
    }
  }
}
