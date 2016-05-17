trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, r) if i == Int.MinValue => (i, r)
      case (i, r) => (Math.abs(i), r)
    }

  def double(rng: RNG): (Double, RNG) =
    nonNegativeInt(rng) match {
      case (i, r) => (i / (Int.MaxValue.toDouble + 1), r)
    }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)
  }


  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case (d, r) => (d.swap, r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case n if n > 0 => {
      val (i, r1) = rng.nextInt
      val (l, r2)  = ints(count - 1)(r1)

      (i :: l, r2)
    }
    case _ => (Nil, rng)
  }

  def doubleWithRand: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble / + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)

      (f(a, b), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDoubleMap2: Rand[(Int, Double)] =
    both(int, double)

  def randIntDoubleIntMap2: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case x::xs => flatMap(sequence(xs))(l => map(x)(a => a :: l))
    case _ => rng => (Nil, rng)
  }

  def sequenceFromBook[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (v, r) = f(rng)
      g(v)(r)
    }

  def mapWithFlatMap[A, B](f: Rand[A])(g: A => B): Rand[B] =
    flatMap(f)(a => unit(g(a)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = ???
//    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
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
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
