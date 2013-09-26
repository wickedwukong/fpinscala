package fpinscala.datastructures

import scala.runtime.RichInt

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(leftTree, rightTree) => size(leftTree) + size(rightTree)
    }
  }

  def max(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(a) => a
      case Branch(leftTree, rightTree) => {
        Tree.max(leftTree) max Tree.max(rightTree)
      }
    }
  }
}