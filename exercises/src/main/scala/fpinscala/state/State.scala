package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapVaiFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    val g: A => Rand[B] = (a: A) => {
      rng => (f(a), rng)
    }
    flatMap(s)(g)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case x@(i, _) if i >= 0 => x
      case (i, rng) if i < 0 => positiveInt(rng)
      case (i, rng) if i == Int.MinValue => positiveInt(rng)
    }
  }

  def doubleViaMap: Rand[Double] = {
    map(positiveInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  def doubleViaMapViaFlatMap: Rand[Double] = {
    mapVaiFlatmap(positiveInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  def double(rng: RNG): (Double, RNG) = {
    def go(value: Double): Double = {
      val dividedByTen = value / 10
      if (dividedByTen >= 1) go(dividedByTen) else dividedByTen
    }

    val valueAndRNG: (Int, RNG) = RNG.positiveInt(rng)

    (go(valueAndRNG._1.toDouble), valueAndRNG._2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val intRng: (Int, RNG) = rng.nextInt
    val doubleRng: (Double, RNG) = double(intRng._2)
    ((intRng._1, doubleRng._1), doubleRng._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    0.until(count).foldLeft((List.empty[Int], rng)) {
      (acc, _) => {
        val (i, nextRng) = acc._2.nextInt
        (i :: acc._1, nextRng)
      }
    }
  }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    val f: Rand[Int] = _.nextInt
    sequence(List.fill(count)(f))(rng)
  }


  def positiveLessThan(n: Int): Rand[Int] =
    flatMap(positiveInt) {
      i: Int =>
        val mod: Int = i % n
        if (i + (n - 1) - mod > 0) (mod, _) else positiveLessThan(n)
    }

  def rollDie: Rand[Int] = {
    RNG.map(positiveLessThan(6))(_ + 1)
  }


  def positiveMax(n: Int): Rand[Int] = sys.error("todo")

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }
  }


  def map2ViaFlatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    val f1: Rand[(A, B)] = rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      ((a, b), rng2)
    }
    val g: ((A, B)) => Rand[C] = (x) => rng => (f(x._1, x._2), rng)
    flatMap(f1)(g)

  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) {
      (f, acc) => map2(f, acc)(_ :: _)
    }


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }
}


import State._

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  )
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] = {
    list.foldRight(unit[S, List[A]](List())) {
      (s, acc) => s.map2(acc)(_ :: _)
    }
  }

  def modify[S](f: S => S): State[S, Unit] = {
    //solution 1: use flatMap
    gets.flatMap(s => sets(f(s)))

    //solution 2: use for comprehension
    // the comprehension is really a syntax sugar for flatMap. the following for comprehension is same as the flatMap above.

    //    for {
    //      s <- gets // Gets the current state and assigns it to `s`.
    //      _ <- sets(f(s)) // Sets the new state to `f` applied to `s`.
    //    } yield ()
  }


  def gets[S]: State[S, S] =
    State(s => (s, s))

  def sets[S](s: S): State[S, Unit] =
    State(_ => ((), s))


  def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
//    solution 1: for comprehension
    for {
        _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _)) => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)
        })))
        s <- gets
      } yield s.coins

    //solution 2: flatMap
//    sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
//      case (_, Machine(_, 0, _)) => s
//      case (Coin, Machine(false, _, _)) => s
//      case (Turn, Machine(true, _, _)) => s
//      case (Coin, Machine(true, candy, coin)) =>
//        Machine(false, candy, coin + 1)
//      case (Turn, Machine(false, candy, coin)) =>
//        Machine(true, candy - 1, coin)
//    }))).flatMap(_ => gets.map(s => s.coins))
  }
}