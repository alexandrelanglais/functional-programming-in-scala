package basics

import org.scalatest._

class EitherTest extends FlatSpec with Matchers {

  "map" should "be able to apply a function to the value of the Right(either)" in {
    val s = MyRight(2)

    s.map(_ * 2) shouldBe MyRight(4)
  }

  "flatMap" should "be able to apply a function to the value of the Right(either)" in {
    val s = MyRight(2)

    s.flatMap(x => MyRight(x * 2)) shouldBe MyRight(4)
  }

  "getOrElse" should "be able to give a default value if option is empty" in {
    pending
    val s = MySome(2)
    val n = MyNone

    s.getOrElse(4) shouldBe 2
    n.getOrElse(2) shouldBe 2
  }

  "orElse" should "be able to give me a right either if the either is left" in {
    val s = MyRight(2)
    val n = MyLeft("coucou")

    s.orElse(s) shouldBe s
    n.orElse(s) shouldBe s
  }

  "map2" should "combine 2 either values wiwth an operation" in {
    val o1 = MyRight(3)
    val o2 = MyRight(4)

    o1.map2(o2)(_ * _) shouldBe MyRight(12)
    o1.map2(o2)(_ + _) shouldBe MyRight(7)
    o2.map2(MyLeft("coucou"))(_ + _) shouldBe MyLeft("coucou")
  }

  "sequence" should "convert a list of either into either the first error or a list" in {
    val l1 = List(MyRight(2), MyRight(3), MyRight(4))
    val l2 = List(MyRight(1))
    val l3 = List(MyRight(1), MyLeft("error"), MyRight(5))
    val l4 = List(MyRight(1), MyLeft("oops"), MyRight(5), MyLeft("oula"))

    MyEither.sequence(l1) shouldBe MyRight(List(2, 3, 4))
    MyEither.sequence(l2) shouldBe MyRight(List(1))
    MyEither.sequence(l3) shouldBe MyLeft("error")
    MyEither.sequence(l4) shouldBe MyLeft("oops")
  }

  "traverse" should "convert a list of either into either the first error or a list" in {
    val l1 = List(MyRight(2), MyRight(3), MyRight(4))
    val l2 = List(MyRight(1))
    val l3 = List(MyRight(1), MyLeft("error"), MyRight(5))

    MyEither.traverse(l1)(x => x) shouldBe MyRight(List(2, 3, 4))

    MyEither.traverse(l3)(x => x) shouldBe MyLeft("error")
  }

  "sequenceAsTraverse" should "the same as sequence" in {
    pending
    val l1 = List(MySome(2), MySome(3), MySome(4))
    val l2 = List(MySome(1))
    val l3 = List(MySome(1), MyNone, MySome(5))

    MyOption.sequenceAsTraverse(l1) shouldBe MyOption.sequence(l1)
    MyOption.sequenceAsTraverse(l2) shouldBe MyOption.sequence(l2)
    MyOption.sequenceAsTraverse(l3) shouldBe MyOption.sequence(l3)
  }

}
