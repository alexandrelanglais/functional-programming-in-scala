package basics.states

import scala.annotation.tailrec

trait RNG {

  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt

  def randomPair: Rand[(Int, Int)] = rng => {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng.nextInt
    ((i1, i2), rng3)
  }

  def nonNegativeInt: Rand[Int] = rng => {
    val (n, r) = rng.nextInt
    val p =
      if (n == Int.MinValue) math.abs(n + 1)
      else math.abs(n)
    (p, r)
  }

  def double: Rand[Double] = rng => {
    val (ri, rng2) = nonNegativeInt(rng)
    val d = ri.toDouble / Int.MaxValue
    (d, rng2)
  }

  def intDouble: Rand[(Int, Double)] = rng => {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt: Rand[(Double, Int)] = rng => {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3: Rand[(Double, Double, Double)] = rng => {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)

    ((d, d2, d3), r3)
  }

  def ints(count: Int): Rand[List[Int]] = rng => {
    def go(n: Int, l: List[Int]): Rand[List[Int]] = r => {
      if (n < count) {
        val (i, r2) = r.nextInt
        go(n + 1, i :: l)(r2)
      } else (l, r)
    }

    go(0, Nil)(rng)
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def elegantDouble: Rand[Double] =
    map(_.nextInt)(i => i.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)

      (f(a, b), rngb)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsSequence(count: Int): Rand[List[Int]] = {
    val l: List[Rand[Int]] = List.fill(count)(int)
    sequence(l)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
