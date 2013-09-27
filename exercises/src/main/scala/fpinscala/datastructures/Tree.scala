package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

//  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B


  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(leftTree, rightTree) => 1 + size(leftTree) + size(rightTree)
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(leftTree, rightTree) => Branch(map(leftTree)(f), map(rightTree)(f))
    }
  }

  def depth[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 0
        case Branch(leftTree, rightTree) =>1 + (depth(leftTree)  max depth(rightTree))
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