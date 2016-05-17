import RNG._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.prop.{PropertyChecks}

class StateSpec extends FunSuite with PropertyChecks {
  object GenRNG {
    def rng: Gen[RNG] = for {
      n <- arbitrary[Int]
    } yield Simple(n)
  }

  object Arbitraries {
    implicit def rngArbitrary: Arbitrary[RNG] = Arbitrary(GenRNG.rng)
  }

  import Arbitraries.rngArbitrary

  test("6.1 - nonNegativeInt") {
    forAll { rng: RNG =>
      nonNegativeInt(rng) match {
        case (n, _) => assert(n >= 0)
      }
    }
  }

  test("6.2 - double") {
    forAll { rng: RNG =>
      double(rng) match {
        case (n, _) => assert(n >= 0 && n < 1)
      }
    }
  }

  test("6.4 - ints") {
    forAll { rng: RNG =>
      ints(100)(rng) match {
        case (l, r) => {
          assert(l.size == 100)
          assert(r != rng)
        }
      }
    }
  }

  test("6.5 - double with Rand") {
    forAll { rng: RNG =>
      doubleWithRand(rng) match {
        case (n, _) => assert(n >= 0 && n < 1)
      }
    }
  }

  test("6.6 - map2") {
    forAll { rng: RNG =>
      forAll(arbitrary[(Int, Int) => Int]) { fn => {
        forAll(arbitrary[(Int, Int)]) { case (n1, n2) => {
          map2(unit(n1), unit(n2))(fn)(rng) match {
            case (n, r) => assert(n == fn(n1, n2))
          }
        }}
      }}
    }
  }

  test("6.7 - sequence") {
    forAll { rng: RNG => {
      forAll(arbitrary[List[Rand[Int]]]) { input: List[Rand[Int]] => {
        val output = sequence(input)

        val l: List[Int] = input.map(i => i(rng)).map(_._1)
        val o: List[Int] = output(rng)._1

        assert(l == o)
      }}
    }}
  }

  test("6.8 - flatMap") {
    forAll { rng: RNG => {
      forAll(arbitrary[Int => Rand[Int]]) { fn => {
        forAll(arbitrary[Int]) { n: Int => {
          val rand: Rand[Int] = unit(n)
          val rand2: Rand[Int] = flatMap(rand)(fn)

          val (i, _) = rand(rng)
          val (d, _) = rand2(rng)

          val (res, _) = fn(i)(rng)

          assert(res == d)
        }}
      }}
    }}
  }

  test("6.9 - mapWithFlatmap") {
    forAll { rng: RNG => {
      forAll(arbitrary[Int => Int]) { fn => {
        forAll(arbitrary[Int]) { n: Int => {
          val rand: Rand[Int] = unit(n)
          val rand2: Rand[Int] = map(rand)(fn)

          val (i, _) = rand(rng)
          val (d, _) = rand2(rng)

          val res = fn(i)

          assert(res == d)
        }}
      }}
    }}
  }

  test("6.9 - map2WithFlatmap") {
    forAll { rng: RNG =>
      forAll(arbitrary[(Int, Int) => Int]) { fn => {
        forAll(arbitrary[(Int, Int)]) { case (n1, n2) => {
          map2WithFlatMap(unit(n1), unit(n2))(fn)(rng) match {
            case (n, r) => assert(n == fn(n1, n2))
          }
        }}
      }}
    }
  }
}
