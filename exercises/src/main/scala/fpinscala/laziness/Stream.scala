package fpinscala.laziness

import scala.annotation.tailrec


trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    import scala.collection.mutable.ListBuffer
    val buffer = ListBuffer[A]()

    @annotation.tailrec
    def go(stream: Stream[A]): List[A] = {
      stream.uncons match {
        case Some((a, tail)) => buffer += a; go(tail)
        case _ => buffer.toList
      }
    }
    go(this)
  }

  def toListViaFoldRight: List[A] = {
    uncons.foldRight(List[A]())((a, list) => {
      (a._1 :: list) ++ a._2.toListViaFoldRight
    })
  }

  def toListViaFlatMap: List[A] = {
    uncons.toList.flatMap {
      case (a, tail) => {
        a :: tail.toListViaFlatMap
      }
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def takeNonTailRec(n: Int): Stream[A] = {
    def go(stream: Stream[A], n: Int): Stream[A] = {
      if (n > 0) {
        stream.uncons match {
          case Some((a, tail)) => Stream.cons(a, go(tail, n - 1))
          case None => Stream.empty
        }
      } else {
        Stream.empty[A]
      }
    }

    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = sys.error("todo")

  def forAll(p: A => Boolean): Boolean = sys.error("todo")
}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] {
      def uncons = None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = sys.error("todo")
}