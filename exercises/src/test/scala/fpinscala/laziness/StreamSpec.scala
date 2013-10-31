package fpinscala.laziness

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "toList" should {
    "convert an empty stream into an empty List" in {
      Stream.empty.toList must_== Nil
      Stream.empty.toListViaFoldRight must_== Nil
      Stream.empty.toListViaFlatMap must_== Nil
    }

    "convert a one-element stream into a one-element List" in {
      Stream(1).toList must_== List(1)
      Stream(1).toListViaFoldRight must_== List(1)
      Stream(1).toListViaFlatMap must_== List(1)
    }

    "convert a two-element stream into a two-element List" in {
      Stream(1, 2).toList must_== List(1, 2)
      Stream(1, 2).toListViaFoldRight must_== List(1, 2)
      Stream(1, 2).toListViaFlatMap must_== List(1, 2)
    }
  }

}
