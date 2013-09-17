package fpinscala.datastructures

import org.specs2.mutable.Specification
import scala.Predef._
import org.specs2.matcher._


class ListSpec extends Specification {
  "drop" should {
    "not drop any element when the 0 elemement is to be dropped" in {
      List.drop(List(1,2), 0) must_== List(1, 2)
    }
    "drop the first element in a multiple element list" in {
      List.drop(List(1,2), 1) must_== List(2)
    }
    "drop the first two elements in a multiple element list" in {
      List.drop(List(1,2, 3), 2) must_== List(3)
    }
    "return an empty list when the list size is same as the number of elements to be dropped" in {
      List.drop(List(1,2), 2) must_== Nil
    }
    "return an empty list when the list size is less than the number of elements to be dropped" in {
      List.drop(List(1,2), 3) must_== Nil
    }




    "return an empty list when the list is Nil" in {
      List.drop(Nil, 0) must_== Nil
      List.drop(Nil, 1) must_== Nil
      List.drop(Nil, 2) must_== Nil
    }
  }

  "tail" should {
    "drop the first element" in {
      List.tail(List(1,2)) must_== List(2)
    }
    "return an empty list when there is only one element" in {
      List.tail(List(1)) must_== Nil
    }
    "return an empty list when the list is empty" in {
      List.tail(Nil) must_== Nil
    }
  }

  "setHead" should {
    "replace first element with new value" in {
      List.setHead(List(1,2))(3) must_== List(3,2)
      List.setHead(List(1))(2) must_== List(2)
      List.setHead(Nil)(1) must_== List(1)
    }
  }

  "dropWhile" should {
    "drop nothing when function always evaluates to false" in {
      List.dropWhile(List(1))(_ => false) must_== List(1)
    }
    "drop everyting when function always evaluates to true" in {
      List.dropWhile(List(1))(_ => true) must_== Nil
    }

    "should only drop values when the function evaluates to true" in {
      List.dropWhile(List(1,2,3))(_ > 1) must_== List(1, 2, 3)
      List.dropWhile(List(1,2,3))(_ > 0) must_== Nil
      List.dropWhile(List(1,2,3))(_ == 1) must_== List(2,3)
      List.dropWhile(List(1,2,3))(_ == 2) must_== List(1,2,3)
    }
  }

  "init" should {
    "return an empty list when the list is empty" in {
      List.init(Nil) must_== Nil
    }

    "return an empty list when there is only one element in the list" in {
      List.init(List(1)) must_== Nil
    }

    "drop the last element" in {
      List.init(List(1,2)) must_== List(1)
      List.init(List(1,2,3)) must_== List(1,2)
    }
  }

  "foldRight" should {
    "test to pass in a Nil and Cons" in {
      List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_)) must_== List(1,2,3)
    }

//    "stack overflow for a large list" in {
//      val largeList: List[Long] = (1L to 100000L).foldLeft(Nil: List[Long])((acc, i) => Cons(i, acc))
//      List.foldRight(largeList, 0L)(_ + _) must_== 1L
//      1 must_== 1
//    }
  }

  "foldLeft" should {
    "be 0 for an empty list" in {
      List.foldLeft(Nil: List[Int], 0)((elem, sum) => elem + sum ) must_== 0
    }

    "be 1 for an one-element list" in {
      List.foldLeft(List(1), 0)((sum, elem) => elem + sum ) must_== 1
    }

    "be 3 for an two-element list" in {
      List.foldLeft(List(1, 2), 0)((sum, elem) => elem + sum ) must_== 3
    }

    "test to pass in a Nil and Cons" in {
      List.foldLeft(List(1, 2, 3), Nil: List[Int])((nil, elem) => Cons(elem, nil)) must_== List(3, 2, 1)
    }

    "should not stack overflow for a large list" in {
      val largeList: List[Long] = (1L to 100000L).foldLeft(Nil: List[Long])((acc, i) => Cons(i, acc))
      List.foldLeft(largeList, 0L)(_ + _) must_== 5000050000L
    }
  }

  "length" should {
    "be 0 for Nil" in {
      List.length(Nil) must_== 0
    }

    "be 1 for one-element List" in {
      List.length(List(1)) must_== 1
    }

    "be 2 for two-element List" in {
      List.length(List(1,2)) must_== 2
    }
  }
}
