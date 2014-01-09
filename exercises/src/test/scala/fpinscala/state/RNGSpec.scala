package fpinscala.state

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck._
import fpinscala.state.RNG._

class RNGSpec extends Specification with ScalaCheck {

  "positiveRNG" should {
    "always generate positive integers" in {
      "different seeds" ! check {
        (seed: Long) => {
          RNG.positiveInt(Simple(seed))._1 must be_>=(0)
        }
      }
    }
  }

  "double" should {
    "genereate a value between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          RNG.double(Simple(seed))._1 must (be_>=(0.0) and be_<(1.0))
        }
      }
    }
  }

  "doubleViaMap" should {
    "genereate a value between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          RNG.doubleViaMap(Simple(seed))._1 must (be_>=(0.0) and be_<(1.0))
          RNG.doubleViaMapViaFlatMap(Simple(seed))._1 must (be_>=(0.0) and be_<(1.0))
        }
      }
    }
  }

  "intDouble" should {
    "genereate an int value and a double value between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val value: ((Int, Double), RNG) = RNG.intDouble(Simple(seed))
          value._1._2 must (be_>=(0.0) and be_<(1.0))
        }
      }
    }
  }

  "map2 generate intDouble" should {
    "genereate an int value and a double value between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val value1: ((Int, Double), RNG) = RNG.map2(rng => RNG.positiveInt(rng), rng => RNG.double(rng))((i, d) => (i, d))(Simple(seed))
          value1._1._2 must (be_>=(0.0) and be_<(1.0))

          val value2: ((Int, Double), RNG) = RNG.map2ViaFlatmap(rng => RNG.positiveInt(rng), rng => RNG.double(rng))((i, d) => (i, d))(Simple(seed))
          value2._1._2 must (be_>=(0.0) and be_<(1.0))
        }
      }
    }

  }

  "doubleInt" should {
    "genereate an int value and a double value between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val value: ((Double, Int), RNG) = RNG.doubleInt(Simple(seed))
          value._1._1 must (be_>=(0.0) and be_<(1.0))
        }
      }
    }
  }

  "ints intsViaSequence" should {
    "genereate 0 ints when the count is 0" in {
      "different seeds" ! check {
        (seed: Long) => {
          val (list1, _) = RNG.ints(0)(Simple(seed))
          list1.length must_== (0)

          val (list2, _) = RNG.intsViaSequence(0)(Simple(seed))
          list2.length must_== (0)
        }
      }
    }

    "genereate 1 int when the count is 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val (list1, _) = RNG.ints(1)(Simple(seed))
          list1.length must_==(1)

          val (list2, _) = RNG.intsViaSequence(1)(Simple(seed))
          list2.length must_==(1)
        }
      }
    }

    "genereate 2 ints when the count is 2 and they are not equal" in {
      "different seeds" ! check {
        (seed: Long) => {
          val (list1, _) = RNG.ints(2)(Simple(seed))
          list1.length must_==(2)
          list1(0) must_!=(list1(1))

          val (list2, _) = RNG.intsViaSequence(2)(Simple(seed))
          list2.length must_==(2)
          list2(0) must_!=(list2(1))
        }
      }
    }
  }

  "double3" should {
    "genereate a3 different double values between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val value: ((Double, Double, Double), RNG) = RNG.double3(Simple(seed))
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

  "sequence" should {
    "genereate a3 different double values between 0 and 1 but not include 1" in {
      "different seeds" ! check {
        (seed: Long) => {
          val value: (List[Int], RNG) = RNG.sequence(List[Rand[Int]](rng => rng.nextInt, rng => rng.nextInt))(Simple(seed))

          val listValues = value._1
          listValues.size must_== 2
          listValues(0) must_!=(listValues(1))
        }
      }
    }
  }

  "rollDie" should {
    "be between 1 and 6 inclusively" in {
      "different seeds" ! check {
        (seed: Long) => {
          val (value, _) = RNG.rollDie(Simple(seed))

          value must (be_>=(1) and be_<=(6))
        }
      }
    }
  }


}
