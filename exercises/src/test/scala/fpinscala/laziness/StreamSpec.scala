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

  "take" should {
    "give an empty stream for 0" in {
      Stream(1).takeNonTailRec(0).toList must_== Stream.empty.toList
      Stream(1).take(0).toList must_== Stream.empty.toList
    }

    "give 1-element stream for 1" in {
      Stream(1,2).takeNonTailRec(1).toList must_== Stream(1).toList
      Stream(1,2).take(1).toList must_== Stream(1).toList
    }

    "give a 2-element stream for 2" in {
      Stream(1,2).takeNonTailRec(2).toList must_== Stream(1,2).toList
      Stream(1,2).take(2).toList must_== Stream(1,2).toList
      Stream(1,2, 3).takeNonTailRec(2).toList must_== Stream(1,2).toList
      Stream(1,2, 3).take(2).toList must_== Stream(1,2).toList
    }

    "give the whole stream for n > the stream length" in {
      Stream(1).takeNonTailRec(2).toList must_== Stream(1).toList
      Stream(1,2).takeNonTailRec(3).toList must_== Stream(1,2).toList
    }
  }

  "takeWhile" should {
    "take no element when f evaluates to false" in {
      Stream(1,2,3).takeWhile(a => false).toList must_== Stream.empty.toList
    }

    "take elements when f evluates to true" in {
      Stream(1,2,3).takeWhile(a => true).toList must_== Stream(1,2,3).toList
      Stream(1,2,3).takeWhile(a => a > 0).toList must_== Stream(1,2,3).toList
      Stream(1,2,3).takeWhile(a => a > 1).toList must_== Stream.empty.toList
      Stream(1,2,3).takeWhile(a => a < 2).toList must_== Stream(1).toList
      Stream(1,2,3).takeWhile(a => a < 3).toList must_== Stream(1, 2).toList
      Stream(1,2,3).takeWhile(a => a == 1).toList must_== Stream(1).toList
      Stream(1,2,3).takeWhile(a => a < 4).toList must_== Stream(1,2,3).toList
    }
  }

}
