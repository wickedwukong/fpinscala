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

  "map2" should {
    "map 2 right to 1 right" in {
      Right(1).map2(Right(2))((a, b) => a + b) must_==Right(3)
    }

    "map 1 right and 1 left to a left" in {
      val left: Either[String, Int] = Left("error")
      left.map2(Right(2))((a, b) => a + b)  must_== Left("error")

      Right(1).map2(left)((a, b) => a + b)  must_== Left("error")

      left.map2(left)((a, b) => a + b) must_== Left("error")
    }
  }

  "sequenceViaFoldLeft" should {
    "convert a List of Right to a Right List" in {
      Either.sequenceViaFoldLeft(List(Right(1))) must_== Right(List(1))
      Either.sequenceViaFoldLeft(List(Right(1), Right(2))) must_== Right(List(1, 2))
    }

    "convert a List of Left to the first Left in the List" in {
      Either.sequenceViaFoldLeft(List(Left("error"))) must_== Left("error")
      Either.sequenceViaFoldLeft(List(Left("error1"), Left("error2"))) must_== Left("error1")
    }

    "convert a List containing a Left to the first Left in the List" in {
      Either.sequenceViaFoldLeft(List(Left("error"), Right(1))) must_== Left("error")
      Either.sequenceViaFoldLeft(List(Right(1), Left("error1"), Left("error2"))) must_== Left("error1")
      Either.sequenceViaFoldLeft(List(Right(1), Left("error1"), Right(2))) must_== Left("error1")
    }
  }

  "map2ViaForComprehension" should {
    "map 2 right to 1 right" in {
      Right(1).map2ViaForComprehension(Right(2))((a, b) => a + b) must_==Right(3)
    }

    "map 1 right and 1 left to a left" in {
      val left: Either[String, Int] = Left("error")
      left.map2ViaForComprehension(Right(2))((a, b) => a + b)  must_== Left("error")

      Right(1).map2ViaForComprehension(left)((a, b) => a + b)  must_== Left("error")

      left.map2ViaForComprehension(left)((a, b) => a + b) must_== Left("error")
    }
  }
}
