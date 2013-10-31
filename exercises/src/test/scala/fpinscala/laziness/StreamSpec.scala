package fpinscala.laziness

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "toList" should {
    "convert an empty stream into an empty List" in {
      Stream.empty[Int] should be (Nil)
    }

    "convert a one-element stream into a one-element List" in {
      Stream(1).toList should be (List(1))
    }

    "convert a two-element stream into a two-element List" in {
      Stream(1, 2).toList should be (List(1, 2))
    }
  }

}
