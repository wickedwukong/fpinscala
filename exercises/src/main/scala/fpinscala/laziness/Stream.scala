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
    case Some((h, t)) if n > 0 => Stream.cons(h, t.takeNonTailRec(n - 1))
    case _ => Stream()
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((n, this)) {
      case (m, originalStream) if (m > 0) => originalStream.uncons.map {
        case (h, t) => (h, (m - 1, t))
      }
      case _ => None

    }
  }

  def takeViaUnfold_1(n: Int): Stream[A] = {
    unfold((n, this)) {
      case (m, originalStream) => originalStream.uncons match {
        case Some((h, t)) if (m > 0) => Some((h, (m - 1, t)))
        case _ => None
      }
    }
  }

  def take(n: Int): Stream[A] = {
    var buffer = ListBuffer[A]()
    @tailrec
    def go(stream: Stream[A], n: Int): Unit = stream.uncons match {
      case Some((a, tail)) if (n > 0) => buffer += a; go(tail, n - 1)
      case _ =>
    }

    go(this, n)

    Stream.apply(buffer: _*)
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this)(stream => stream.uncons match {
      case s@Some((h, _)) if (p(h)) => s
      case _ => None
    })
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
    foldRight(Stream.empty[A]) {
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
    foldRight(Stream.empty[B]) {
      (a, stream) => cons(f(a), stream)
    }
  }

  def mapViaUnfold[B](f: => A => B): Stream[B] = unfold(this)(
    stream => stream.uncons match {
      case Some((a, s)) => Some(f(a), s)
      case _ => None
    }
  )


  def filter(f: => A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) {
      (a, stream) => if (f(a)) cons(a, stream) else stream
    }
  }

  def append[B >: A](streamB: Stream[B]): Stream[B] = foldRight(streamB) {
    (a, stream) => cons(a, stream)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => f(h).foldRight(t)((h1, t1) => cons(h1, t1)))
    //    foldRight(empty[B])((h,t) => f(h) append t)
  }

  def find(p: A => Boolean): Option[A] = filter(p).uncons.map(_._1)

  def zipWith[B, C](s2: Stream[B])(f: => (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (s1, s2) => (s1.uncons, s2.uncons) match {
        case (Some((h1, t1)), Some((h2, t2))) => Some(f(h1, h2), (t1, t2))
        case _ => None
      }
    }
  }

  def zip[B](s2: Stream[B]): Stream[(A, B)] = {
    zipWith(s2)((_,_))
  }

  def zipAll[B](s2: Stream[B]):Stream[(Option[A], Option[B])] = ???
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

  val ones: Stream[Int] = constant(1)

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  //  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def constant[A](a: A): Stream[A] = new Stream[A] {
    def uncons = Some((a, this))
  }

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))


  def from(n: Int): Stream[Int] = new Stream[Int] {
    def uncons = Some((n, from(n + 1)))
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def fibs: Stream[Int] = {

    def go(state: Int, nextVal: Int): Stream[Int] = new Stream[Int] {
      def uncons = Some((state, go(nextVal, state + nextVal)))

    }

    go(0, 1)
  }

  def fibsViaUnfold: Stream[Int] = {
    cons(0, unfold((0, 1)) {
      case (a, b) => Some((b, (b, a + b)))
    })
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Stream.empty[A]
    }
  }

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = sys.error("todo")
}