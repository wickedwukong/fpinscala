package fpinscala.datastructures

import org.specs2.mutable.Specification
import Tree._
import scala.Predef._

class TreeSpec extends Specification {
  "size" should {
    "give 1 for a Leaf" in {
      Tree.size(Leaf(1)) must_== 1
    }

    "give 2 for a two Leaf tree" in {
      Tree.size(Branch(Leaf(1), Leaf(2))) must_== 2
    }

    "count the size of a nested tree" in {
      Tree.size(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) must_== 4
    }
  }
}
