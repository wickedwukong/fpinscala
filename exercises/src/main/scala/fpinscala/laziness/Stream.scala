package fpinscala.laziness

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import Stream._


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

  def takeNonTailRec(n: Int): Stream[A] = uncons match {
    case Some((h,t)) if n > 0 => Stream.cons(h, t.takeNonTailRec(n-1))
    case _ => Stream()
  }

  def take(n: Int): Stream[A] = {
    var buffer = ListBuffer[A]()
    @tailrec
    def go(stream: Stream[A], n: Int): Unit = stream.uncons match {
      case Some((a, tail)) if (n > 0) => buffer += a; go(tail, n-1)
      case _ =>
    }

    go(this, n)

    Stream.apply(buffer: _*)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    uncons match {
      case Some((a, tail)) => {
        if (p(a))
          Stream.cons(a, tail.takeWhile(p))
        else
          Stream.empty[A]
      }
      case _ => Stream.empty[A]
    }
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]){
      (a, stream) => if (p(a)) Stream.cons(a, stream) else empty
    }
  }


  def forAll(p: A => Boolean): Boolean = uncons match {
    case Some((a, tail)) => p(a) && tail.forAll(p)
    case _ => true
  }

  def forAllViaFoldRight(p: A => Boolean): Boolean = {
    foldRight(true)((a, acc) => p(a) && acc)
  }

  def map[B](f: => A => B): Stream[B] = {
    foldRight(Stream.empty[B]){
      (a, stream) => cons(f(a), stream)
    }
  }

  def filter(f: => A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]){
      (a, stream) => if (f(a)) cons(a, stream) else stream
    }
  }
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