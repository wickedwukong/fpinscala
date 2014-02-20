package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._

import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))

  def boolean: Gen[Boolean] = Gen(State(RNG.positiveInt).map(i => i % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    if (start == stopExclusive) {
      Gen(State(RNG.positiveInt).map(_ => start))
    } else {
      Gen(State(RNG.positiveInt).map(i => start + i % (stopExclusive - start)))
    }
  }

  def chooseTwo(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    listOfN(2, choose(start, stopExclusive)).map(l => (l.head, l.tail.head))
  }

  def sameParity(from: Int, to: Int): Gen[(Int,Int)] = {
    for {
      i <- choose(from, to)
      j <- if (i%2 == 0) even(from,to) else odd(from,to)
    } yield(i, j)
  }

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 == 0) stopExclusive - 1 else stopExclusive).
      map (n => if (n%2 != 0) n+1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 != 0) stopExclusive - 1 else stopExclusive).
      map (n => if (n%2 == 0) n+1 else n)

}

case class Gen[+A](sample: State[RNG, A]) {

  def union(g1: Gen[A], g2: Gen[A]) : Gen[A] = {
    Gen.boolean.flatMap(b => if (b) g1 else g2)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }


  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))


  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
}


//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

