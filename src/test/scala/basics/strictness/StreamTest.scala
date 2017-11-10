package basics.strictness

import basics.MyList
import org.scalatest._

class StreamTest extends FlatSpec with Matchers {

  "toList" should "convert a stream to a list" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

    s.toList shouldBe List(1, 2, 3)
  }

  "take" should "take n first elements from a stream" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

    s.take(2).toList shouldBe List(1, 2)
    s.take(1).toList shouldBe List(1)
    s.take(0).toList shouldBe Nil
  }

  "drop" should "skip the n first elements from a stream" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

    s.drop(2).toList shouldBe List(3)
    s.drop(1).toList shouldBe List(2, 3)
    s.drop(0).toList shouldBe List(1, 2, 3)
  }

  "takeWhile" should "take the first elements of a stream while predicate satisfied" in {
    val s = Stream.cons(2, Stream.cons(4, Stream.cons(5, Stream.empty)))

    s.takeWhile(_ % 2 == 0).toList shouldBe List(2, 4)
    s.takeWhile(_ % 2 != 0).toList shouldBe Nil
    s.takeWhile(_ < 3).toList shouldBe List(2)
  }

  "forall" should "check a condition for every element of a stream" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
    val s2 = Stream.cons(2, Stream.cons(4, Stream.cons(6, Stream.empty)))

    s.forAll(_ % 2 == 0) shouldBe false
    s2.forAll(_ % 2 == 0) shouldBe true
  }

  "takeWhileWithFoldRight" should "work like takeWhile" in {
    val s = Stream.cons(2, Stream.cons(4, Stream.cons(5, Stream.empty)))

    s.takeWhileWithFoldRight(_ % 2 == 0).toList shouldBe List(2, 4)
    s.takeWhileWithFoldRight(_ % 2 != 0).toList shouldBe Nil
    s.takeWhileWithFoldRight(_ < 3).toList shouldBe List(2)
  }

  "headOptionWithFoldRight" should "return an option containing the head of the stream" in {
    val s = Stream.cons(2, Stream.cons(4, Stream.cons(5, Stream.empty)))
    val e = Empty

    s.headOptionWithFoldRight shouldBe Some(2)
    e.headOptionWithFoldRight shouldBe None
  }

  "map" should "modify a stream maintaining its structure" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

    s.map(_ * 2).toList shouldBe List(2, 4, 6)
  }

  "filter" should "filter elements of a stream" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

    s.filter(_ % 2 == 0).toList shouldBe List(2)
    s.filter(_ % 2 != 0).toList shouldBe List(1, 3)
  }

  "append" should "append a stream to another" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
    val s2 = Stream.cons(2, Stream.cons(4, Stream.cons(5, Stream.empty)))

    s.append(s2).toList shouldBe List(1, 2, 3, 2, 4, 5)
  }

  "flatmap" should "return a flat stream" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

    s.flatMap(i => Stream.cons(i + i, Empty)).toList shouldBe List(2, 4, 6)
  }

  "constant" should "return an infinite stream" in {
    val s = Stream.constant(1)

    s.take(1).toList shouldBe List(1)
    s.take(10).toList shouldBe (1 to 10).toList.map(x => 1)
  }

  "from" should "return an infinite stream of integers" in {
    val s = Stream.from(1)

    s.take(1).toList shouldBe List(1)
    s.take(10).toList shouldBe (1 to 10).toList
    s.take(50).toList shouldBe (1 to 50).toList
  }

  "fibs" should "return an infinite fibonacci stream" in {
    val s = Stream.fibs()

    s.take(5).toList shouldBe List(0, 1, 1, 2, 3)
    s.take(8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)

    println(s.take(20).toList)
  }

  "unfold" should "be able to produce a from-like stream" in {
    val s = Stream.unfold(0)((s) => Some((s, s + 1)))

    s.take(5).toList shouldBe List(0, 1, 2, 3, 4)
  }

  "constantU" should "return an infinite stream" in {
    val s = Stream.constantU(1)

    s.take(1).toList shouldBe List(1)
    s.take(10).toList shouldBe (1 to 10).toList.map(x => 1)
  }

  "onesU" should "return an infinite stream" in {
    val s = Stream.onesU()

    s.take(1).toList shouldBe List(1)
    s.take(10).toList shouldBe (1 to 10).toList.map(x => 1)
  }

  "fromU" should "return an infinite stream of integers" in {
    val s = Stream.fromU(1)

    s.take(1).toList shouldBe List(1)
    s.take(10).toList shouldBe (1 to 10).toList
    s.take(50).toList shouldBe (1 to 50).toList
  }

  "fibsU" should "return an infinite fibonacci stream" in {
    val s = Stream.fibsU()

    s.take(5).toList shouldBe List(1, 1, 2, 3, 5)
    s.take(8).toList shouldBe List(1, 1, 2, 3, 5, 8, 13, 21)

    println(s.take(20).toList)
  }

  "mapU" should "modify a stream maintaining its structure" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

    s.mapU(_ * 2).toList shouldBe List(2, 4, 6)
  }

  "takeU" should "take n first elements from a stream" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

    s.takeU(2).toList shouldBe List(1, 2)
    s.takeU(1).toList shouldBe List(1)
    s.takeU(0).toList shouldBe Nil
  }

  "takeWhileU" should "take the first elements of a stream while predicate satisfied" in {
    val s = Stream.cons(2, Stream.cons(4, Stream.cons(5, Stream.empty)))

    s.takeWhileU(_ % 2 == 0).toList shouldBe List(2, 4)
    s.takeWhileU(_ % 2 != 0).toList shouldBe Nil
    s.takeWhileU(_ < 3).toList shouldBe List(2)
  }

  "zipWith" should "be able to perform various operation on two streams" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
    val s2 = Stream.cons(4, Stream.cons(-2, Stream.cons(3, Stream.empty)))

    s.zipWith(s2)(_ + _).toList shouldBe List(5, 0, 6)
  }

  "tails" should "return a stream of the tails of a given stream" in {
    pending
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
    s.tails.toList shouldBe List(List(1, 2, 3), List(2, 3), List(3))

  }
}
