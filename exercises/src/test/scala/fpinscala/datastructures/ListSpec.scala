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
}
