package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
        ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
        simple(seed2))
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

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, Int] = sys.error("todo")
}