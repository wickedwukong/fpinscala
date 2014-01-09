package fpinscala.errorhandling


import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  /*
  Again, we can implement this with explicit pattern matching. 
  */
  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // The `val x: Int = ...` declares `x` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block, as we've seen before. `case e: Exception` is a pattern matching any `Throwable` that is an exception, and which binds this value to the identifier `e`.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A `throw` Exception can be given any type, here we are annotating it with the type `Int`.
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches) // The details of this API don't matter too much, but `p.matcher(s).matches` will check if the string `s` matches the pattern `p`.

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f =>
      mkMatcher(pat2) map (g =>
        f(s) && g(s)))

  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aValue => b.map(bValue => f(aValue, bValue)))
  }

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = sys.error("todo")

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case head :: tail => { head.flatMap(hh => sequence(tail).map(hh :: _))}
    }
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((optionA, acc) => {
      optionA.flatMap(valueA => acc.map(accList => valueA :: accList))
    })
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((item, acc) => {
      f(item).flatMap(b => acc.map(b :: _))
    })
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(a => a)
  }

  def traverseViaSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(a.map(f))
  }
}