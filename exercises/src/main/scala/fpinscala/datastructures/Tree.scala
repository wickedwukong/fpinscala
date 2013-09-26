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

  def depth[A](tree: Tree[A]): Int = {

    def go(tree: Tree[A], currentDepth: Int): Int = {
      tree match {
        case Leaf(_) => 1 + currentDepth
        case Branch(leftTree, rightTree) => (go(leftTree, currentDepth + 1)) max (go(rightTree, currentDepth + 1))
      }
    }

    go(tree, 0)

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