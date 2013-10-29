package fpinscala.demo

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = sys.error("todo")

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = sys.error("todo")

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = sys.error("todo")

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = sys.error("todo")

  def map2ViaForComprehension[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???

}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def sequenceViaFoldLeft[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }

}
