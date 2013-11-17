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
      Stream(1, 2).takeNonTailRec(1).toList must_== Stream(1).toList
      Stream(1, 2).take(1).toList must_== Stream(1).toList
    }

    "give a 2-element stream for 2" in {
      Stream(1, 2).takeNonTailRec(2).toList must_== Stream(1, 2).toList
      Stream(1, 2).take(2).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).takeNonTailRec(2).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).take(2).toList must_== Stream(1, 2).toList
    }

    "give the whole stream for n > the stream length" in {
      Stream(1).takeNonTailRec(2).toList must_== Stream(1).toList
      Stream(1, 2).takeNonTailRec(3).toList must_== Stream(1, 2).toList
    }
  }

  "takeWhile and takeWhileViaFoldRight" should {
    "take no element when f evaluates to false" in {
      Stream(1, 2, 3).takeWhile(a => false).toList must_== Stream.empty.toList
      Stream(1, 2, 3).takeWhileViaFoldRight(a => false).toList must_== Stream.empty.toList
    }

    "take elements when f evaluates to true" in {
      Stream(1, 2, 3).takeWhile(a => true).toList must_== Stream(1, 2, 3).toList
      Stream(1, 2, 3).takeWhile(a => a > 0).toList must_== Stream(1, 2, 3).toList
      Stream(1, 2, 3).takeWhile(a => a > 1).toList must_== Stream.empty.toList
      Stream(1, 2, 3).takeWhile(a => a < 2).toList must_== Stream(1).toList
      Stream(1, 2, 3).takeWhile(a => a < 3).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).takeWhile(a => a == 1).toList must_== Stream(1).toList
      Stream(1, 2, 3).takeWhile(a => a < 4).toList must_== Stream(1, 2, 3).toList

      Stream(1, 2, 3).takeWhileViaFoldRight(a => true).toList must_== Stream(1, 2, 3).toList
      Stream(1, 2, 3).takeWhileViaFoldRight(a => a > 0).toList must_== Stream(1, 2, 3).toList
      Stream(1, 2, 3).takeWhileViaFoldRight(a => a > 1).toList must_== Stream.empty.toList
      Stream(1, 2, 3).takeWhileViaFoldRight(a => a < 2).toList must_== Stream(1).toList
      Stream(1, 2, 3).takeWhileViaFoldRight(a => a < 3).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).takeWhileViaFoldRight(a => a == 1).toList must_== Stream(1).toList
      Stream(1, 2, 3).takeWhileViaFoldRight(a => a < 4).toList must_== Stream(1, 2, 3).toList
    }
  }


  "forAll" should {
    "be true for empty stream" in {
      Stream.empty[Int].forAll((_) => true) must_== true
      Stream.empty[Int].forAllViaFoldRight((_) => true) must_== true
    }

    "be true when every element evaluates to true" in {
      Stream(1, 2, 3).forAll(Int => true) must_== true
      Stream(1, 2, 3).forAllViaFoldRight(Int => true) must_== true
    }

    "be false when at least one element evaluates to false" in {
      Stream(1, 2, 3).forAll((_) => false) must_== false
      Stream(1, 2, 3).forAllViaFoldRight((_) => false) must_== false
      Stream(1, 2, 3).forAll((a) => {
        if (a == 1) false else true
      }) must_== false
      Stream(1, 2, 3).forAllViaFoldRight((a) => {
        if (a == 1) false else true
      }) must_== false
      Stream(1, 2, 3).forAll((a) => if (a == 2) false else true) must_== false
      Stream(1, 2, 3).forAllViaFoldRight((a) => if (a == 2) false else true) must_== false
      Stream(1, 2, 3).forAll((a) => if (a == 3) false else true) must_== false
      Stream(1, 2, 3).forAllViaFoldRight((a) => if (a == 3) false else true) must_== false
    }
  }

  "map" should {
    "transform an empty stream to an empty stream" in {
      Stream.empty[Int].map(_ => "hello").toList must_== Nil
    }

    "transform an non-empty stream" in {
      Stream(1, 2, 3).map(_ + 10).toList must_== List(11, 12, 13)
    }
  }

  "filter" should {
    "include items evaluate to true, and exclude items evaluate to false" in {
      Stream(1,2,3).filter(_ => true).toList must_== List(1,2,3)
      Stream(1,2,3).filter(_ => false).toList must_== Nil
      Stream(1,2,3).filter(a => a > 2).toList must_== List(3)
    }
  }

  "append" should {
    "append empty stream" in {
      Stream.empty[Int].append(Stream.empty[Int]).toList must_== Nil
      Stream(1).append(Stream.empty[Int]).toList must_== List(1)
      Stream(1,2).append(Stream.empty[Int]).toList must_== List(1,2)
    }

    "append non-empty stream" in {
      Stream.empty[Int].append(Stream(1)).toList must_== List(1)
      Stream(1).append(Stream(2)).toList must_== List(1,2)
      Stream(1).append(Stream(2,3)).toList must_== List(1,2,3)
      Stream(1,2).append(Stream(3,4)).toList must_== List(1,2,3,4)
    }
  }

  "flatMap" should {
    "transform empty stream to empty stream" in {
      Stream.empty[Int].flatMap(a => Stream("abc")).toList must_== Nil
    }

    "transform and flatten stream" in {
      Stream(1,2).flatMap(a => Stream("a")).toList must_== List("a", "a")
      Stream(1,2).flatMap(a => Stream(a, a + 1)).toList must_== List(1, 2, 2, 3)
      Stream(1,2).flatMap(a => Stream(s"${a}", s"${a + 1}")).toList must_== List("1", "2", "2", "3")
    }

    "not contain empty stream" in {
      Stream(1,2).flatMap(a => Stream.empty).toList must_== Nil
      Stream(1,2).flatMap(a => if (a > 1) Stream.empty else Stream(a, a + 1)).toList must_== List(1, 2)
    }
  }

  "constant" should {
    "give constantants" in {
      Stream.constant(1).take(5).toList must_== List(1,1,1,1,1)
      Stream.constant("a").take(3).toList must_== List("a", "a", "a")
    }
  }

}
