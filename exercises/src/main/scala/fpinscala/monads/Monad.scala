package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import fpinscala.monads

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def cofactor[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = ???
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    lma.foldRight(unit(List[A]())){
      (ma, acc) => map2(ma, acc)((a, list) => a :: list)
    }
  }

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    sequence(List.fill[M[A]](n)(ma))
  }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = {
    a => flatMap(f(a))(g)
  }

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???

  def join[A](mma: M[M[A]]): M[A] = ???

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    ms match {
      case Nil => unit(ms)
      case head :: tail => flatMap(f(head))(b => {
        if (!b) filterM(tail)(f)
        else map(filterM(tail)(f))(l => head :: l)
      })
    }
  }

}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par]{
    override def unit[A](a: => A) = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = {
      es => f(ma(es).get)(es)
    }

  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)
    override def flatMap[A,B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = ???

  val streamMonad: Monad[Stream] = ???

  val listMonad: Monad[List] = ???

  def stateMonad[S] = ???

  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}




