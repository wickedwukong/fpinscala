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
      Stream(1).takeViaUnfold(0).toList must_== Stream.empty.toList
    }

    "give 1-element stream for 1" in {
      Stream(1, 2).takeNonTailRec(1).toList must_== Stream(1).toList
      Stream(1, 2).take(1).toList must_== Stream(1).toList
      Stream(1, 2).takeViaUnfold(1).toList must_== Stream(1).toList
      Stream(1, 2).takeViaUnfold_1(1).toList must_== Stream(1).toList
    }

    "give a 2-element stream for 2" in {
      Stream(1, 2).takeNonTailRec(2).toList must_== Stream(1, 2).toList
      Stream(1, 2).take(2).toList must_== Stream(1, 2).toList
      Stream(1, 2).takeViaUnfold(2).toList must_== Stream(1, 2).toList
      Stream(1, 2).takeViaUnfold_1(2).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).takeNonTailRec(2).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).take(2).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).takeViaUnfold(2).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).takeViaUnfold_1(2).toList must_== Stream(1, 2).toList
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
      Stream(1, 2, 3).takeWhileViaUnfold(a => false).toList must_== Stream.empty.toList
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

      Stream(1, 2, 3).takeWhileViaUnfold(a => true).toList must_== Stream(1, 2, 3).toList
      Stream(1, 2, 3).takeWhileViaUnfold(a => a > 0).toList must_== Stream(1, 2, 3).toList
      Stream(1, 2, 3).takeWhileViaUnfold(a => a > 1).toList must_== Stream.empty.toList
      Stream(1, 2, 3).takeWhileViaUnfold(a => a < 2).toList must_== Stream(1).toList
      Stream(1, 2, 3).takeWhileViaUnfold(a => a < 3).toList must_== Stream(1, 2).toList
      Stream(1, 2, 3).takeWhileViaUnfold(a => a == 1).toList must_== Stream(1).toList
      Stream(1, 2, 3).takeWhileViaUnfold(a => a < 4).toList must_== Stream(1, 2, 3).toList

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
      Stream.empty[Int].mapViaUnfold(_ => "hello").toList must_== Nil
    }

    "transform an non-empty stream" in {
      Stream(1, 2, 3).map(_ + 10).toList must_== List(11, 12, 13)
      Stream(1, 2, 3).mapViaUnfold(_ + 10).toList must_== List(11, 12, 13)
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
      Stream.constantViaUnfold(1).take(5).toList must_== List(1,1,1,1,1)
      Stream.constant("a").take(3).toList must_== List("a", "a", "a")
      Stream.constantViaUnfold("a").take(3).toList must_== List("a", "a", "a")
    }
  }

  "from" should {
    "start from an initial number" in {
      Stream.from(1).take(1).toList must_== List(1)
      Stream.fromViaUnfold(1).take(1).toList must_== List(1)
      Stream.from(100).take(5).toList must_== List(100, 101, 102, 103, 104)
      Stream.fromViaUnfold(100).take(5).toList must_== List(100, 101, 102, 103, 104)
      Stream.from(100).take(100).toList.size must_== 100
      Stream.fromViaUnfold(100).take(100).toList.size must_== 100
      Stream.from(100).take(100).toList(99) must_== 199
      Stream.fromViaUnfold(100).take(100).toList(99) must_== 199
      Stream.from(1).take(1000).toList(1000 - 1) must_== 1000
      Stream.fromViaUnfold(1).take(1000).toList(1000 - 1) must_== 1000
    }
  }

  "fibs" should {
    "do this" in {
      Stream.fibs.take(1).toList must_== List(0)
      Stream.fibsViaUnfold.take(1).toList must_== List(0)
      Stream.fibs.take(2).toList must_== List(0, 1)
      Stream.fibsViaUnfold.take(2).toList must_== List(0, 1)
      Stream.fibs.take(3).toList must_== List(0, 1, 1)
      Stream.fibsViaUnfold.take(3).toList must_== List(0, 1, 1)
      Stream.fibs.take(4).toList must_== List(0, 1, 1, 2)
      Stream.fibsViaUnfold.take(4).toList must_== List(0, 1, 1, 2)
      Stream.fibs.take(5).toList must_== List(0, 1, 1, 2, 3)
      Stream.fibsViaUnfold.take(5).toList must_== List(0, 1, 1, 2, 3)
      Stream.fibs.take(6).toList must_== List(0, 1, 1, 2, 3, 5)
      Stream.fibsViaUnfold.take(6).toList must_== List(0, 1, 1, 2, 3, 5)
      Stream.fibs.take(7).toList must_== List(0, 1, 1, 2, 3, 5,8)
      Stream.fibsViaUnfold.take(7).toList must_== List(0, 1, 1, 2, 3, 5,8)

      val longFibList = Stream.fibsViaUnfold.take(200).toList

      longFibList(199) must_== longFibList(197) + longFibList(198)
    }
  }

  "zip" should {
    "zip two empty stream" in {
      Stream.empty[Int].zip(Stream.empty[String]).toList must_== List[(Int, String)]()
    }

    "zip two Steam of equal number of elements" in {
      Stream(1).zip(Stream("a")).toList must_== List((1, "a"))
      Stream(1,2).zip(Stream("a", "b")).toList must_== List((1, "a"), (2,"b"))
    }

    "zip two Steam of non-equal number of elements" in {
      Stream(1,2).zip(Stream.empty[String]).toList must_== List[(Int, String)]()
      Stream(1,2).zip(Stream("a")).toList must_== List((1, "a"))
      Stream(1,2,3).zip(Stream("a", "b")).toList must_== List((1, "a"),(2,"b"))
      Stream(1,2).zip(Stream("a", "b", "c")).toList must_== List((1, "a"),(2,"b"))
    }

    "zip two Steam of infinite number of elements" in {
      Stream.ones.zip(Stream.constant("a")).take(5).toList must_== List((1,"a"),(1,"a"),(1,"a"),(1,"a"),(1,"a"))
    }
  }

  "zipAl" should {
    "zip two empty stream" in {
      Stream.empty[Int].zip(Stream.empty[String]).toList must_== List[(Option[Int], Option[String])]()
    }

    "zip one non empty stream and one empty stream" in {
      Stream(1).zip(Stream.empty[String]).toList must_== List[(Option[Int], Option[String])]((Some(1), None))
      Stream(1,2).zip(Stream.empty[String]).toList must_== List[(Option[Int], Option[String])]((Some(1), None),(Some(2), None))
    }
  }

}
