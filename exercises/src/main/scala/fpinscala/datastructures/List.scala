package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

// `List` data type
case object Nil extends List[Nothing]

// data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, List[A]())((acc, ll) => {
      append(acc, ll)
    })
  }

  def appendViaFoldRight[A](l: List[A], i: A): List[A] = {
    foldRight(l, List(i))((c, acc) => Cons(c, acc))
  }

  // `List` companion object
  def sum(ints: List[Int]): Int = ints match {
    // Pattern matching example
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  // Creating lists
  val example2 = List(1, 2, 3)
  val total = sum(example)

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = {
    List.drop(l, 1)
    //    l match {
    //      case Nil => Nil
    //      case Cons(_, xs) => xs
    //    }

  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else l
    }
  }

  def setHead[A](l: List[A])(h: A): List[A] = {
    Cons(h, List.tail(l))
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def length[A](l: List[A]): Int = {
    List.foldRight(l, 0)((elem, acc) => acc + 1)
  }

  def sumUsingFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def reverse[A](l: List[A]): List[A] = {
    List.foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    List.foldRight(l, z)((a, b) => f(b, a))
  }

  def foldLeftViaFoldRight2[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
    List.foldRight(l, (b: B) => b)((a, b) => c => b(f(c, a)))(z)
  }


  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(xs, ys) => foldLeft(ys, f(z, xs))(f)
    }
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    List.reverse(List.foldLeft(l, Nil: List[B])((seed, elem) => Cons(f(elem), seed)))
  }
}