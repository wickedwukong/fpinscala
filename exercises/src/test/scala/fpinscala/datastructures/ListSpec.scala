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
    "should drop nothing when function always evaluates to false" in {
      List.dropWhile(List(1))(_ => false) must_== List(1)
    }
    "should drop everyting when function always evaluates to true" in {
      List.dropWhile(List(1))(_ => true) must_== Nil
    }

    "should only drop values when the function evaluates to true" in {
      List.dropWhile(List(1,2,3))(_ > 1) must_== List(1, 2, 3)
      List.dropWhile(List(1,2,3))(_ > 0) must_== Nil
      List.dropWhile(List(1,2,3))(_ == 1) must_== List(2,3)
      List.dropWhile(List(1,2,3))(_ == 2) must_== List(1,2,3)
    }
  }
}
