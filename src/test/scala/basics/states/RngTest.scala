package basics.states

import org.scalatest._

class RngTest extends FlatSpec with Matchers {

  val rng = new SimpleRNG(515L)

  "nonNegativeValue" should "return a positive value" in {
    assert(rng.nonNegativeInt(rng)._1 > 0)
  }

  "double" should "return a value between 0 and 1" in {
    val (n, _) = rng.double(rng)
    assert(0 < n && n < 1)
  }

  "intDouble" should "generate a random pair of a double and a int" in {
    val ((i, d), r) = rng.intDouble(rng)

    assert(0 < d && d < 1)
    assert(i != d)
  }

  "doubleInt" should "generate a random pair of a double and a int" in {
    val ((d, i), r) = rng.doubleInt(rng)

    assert(0 < d && d < 1)
    assert(i != d)
  }

  "double3" should "generate a triplet of doubles" in {
    val ((d, d2, d3), r) = rng.double3(rng)

    assert(0 < d && d < 1)
    assert(0 < d2 && d2 < 1)
    assert(0 < d3 && d3 < 1)
    assert(d != d2 && d2 != d3)
  }

  "ints" should "generate a list of random numbers" in {
    val (l, r) = rng.ints(5)(rng)

    println(l)
    assert(l.size == 5)
    assert(l.toSet.size == 5)
  }

  "elegantDouble" should "return a value between 0 and 1" in {
    val (n, r) = rng.elegantDouble(rng)

    assert(0 < n && n < 1)
  }

  "randIntDouble" should "generate a random pair of a double and a int" in {
    val ((i, d), r) = rng.randIntDouble(rng)

    assert(0 < d && d < 1)
    assert(i != d)
  }

  "randDoubleInt" should "generate a random pair of a double and a int" in {
    val ((d, i), r) = rng.randDoubleInt(rng)

    assert(0 < d && d < 1)
    assert(i != d)
  }

  "intsSequence" should "generate a list of random numbers" in {
    val (l, r) = rng.intsSequence(5)(rng)

    println(l)
    assert(l.size == 5)
    assert(l.toSet.size == 5)
  }

}
