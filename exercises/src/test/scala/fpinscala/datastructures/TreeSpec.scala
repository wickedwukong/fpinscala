package fpinscala.datastructures

import org.specs2.mutable.Specification

class TreeSpec extends Specification {
  "size" should {
    "give 1 for a Leaf" in {
      Tree.size(Leaf(1)) must_== 1
    }

    "give 2 for a two Leaf tree" in {
      Tree.size(Branch(Leaf(1), Leaf(2))) must_== 3
    }

    "count the size of a nested tree" in {
      Tree.size(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))) must_== 7
    }
  }

  "max" should {
    "give the value of the single leaf tree" in {
      Tree.max(Leaf(100)) must_== 100
    }

    "give the higher value of a two leaf tree" in {
      Tree.max(Branch(Leaf(100), Leaf(1))) must_== 100
      Tree.max(Branch(Leaf(100), Leaf(100))) must_== 100
      Tree.max(Branch(Leaf(1), Leaf(100))) must_== 100
    }

    "give the higest value of a nested tree" in {
      Tree.max(Branch(Leaf(100), Branch(Leaf(200), Leaf(300)))) must_== 300
      Tree.max(Branch(Branch(Leaf(200), Leaf(300)), Leaf(100))) must_== 300
      Tree.max(Branch(Branch(Leaf(200), Leaf(300)), Branch(Leaf(200), Leaf(400)))) must_== 400
    }
  }

  "depth" should {
    "be 1 for a single leaf tree" in {
      Tree.depth(Leaf(100)) must_== 0
    }

    "be 1 for a symmetric one leaf tree" in {
      Tree.depth(Branch(Leaf(100), Leaf(200))) must_== 1
    }

    "be the maximum length of a nested tree" in {
      Tree.depth(Branch(Leaf(100), Branch(Leaf(1), Leaf(2)))) must_== 2
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(100))) must_== 2
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))) must_== 2
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Branch(Leaf(1), Leaf(2))))) must_== 3
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(1), Leaf(2)), Leaf(1)))) must_== 3
      Tree.depth(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(1)), Branch(Leaf(1), Leaf(2)))) must_== 3
    }
  }
}
