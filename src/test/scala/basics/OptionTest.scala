package basics

import org.scalatest._

class OptionTest extends FlatSpec with Matchers {

  "Map" should "be able to apply a function to the value of the option" in {
    val s = MySome(2)

    s.map(_ * 2) shouldBe MySome(4)
  }

  "FlatMap" should "be able to apply a function to the value of the option" in {
    val s = MySome(2)

    s.flatMap(x => MySome(x * 2)) shouldBe MySome(4)
  }

  "getOrElse" should "be able to give a default value if option is empty" in {
    val s = MySome(2)
    val n = MyNone

    s.getOrElse(4) shouldBe 2
    n.getOrElse(2) shouldBe 2
  }

  "orElse" should "be able to give me another option if the option is empty" in {
    val s = MySome(2)
    val n = MyNone

    s.orElse(MySome(4)) shouldBe MySome(2)
    n.orElse(MySome(2)) shouldBe MySome(2)
  }

  "filter" should "give me be back the option if predicate is satisfied" in {
    val s = MySome(2)
    val s1 = MySome(3)


    s.filter(_ % 2 == 0) shouldBe MySome(2)
    s1.filter(_ % 2 == 0) shouldBe MyNone
  }

  "variance" should "calculate the variance of a mean" in {
    val seq:Seq[Double] = Seq(2,3,8,9,15)
    MySome(BigDecimal(MyOptionAnnexes.variance(seq).getOrElse(0.0)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble) shouldBe MySome(21.84)
  }

  "map2" should "combine 2 options values wiwth an operation" in {
    val o1 = MySome(3)
    val o2 = MySome(4)

    MyOption.map2(o1, o2)(_ * _) shouldBe MySome(12)
    MyOption.map2(o1, o2)(_ + _) shouldBe MySome(7)
    MyOption.map2(o2, MyNone)(_ + _) shouldBe MyNone
    MyOption.map2(MyNone, o1)((a, b) => println("yo")) shouldBe MyNone
    MyOption.map2(MyNone, MyNone)((a, b) => println("yo")) shouldBe MyNone
  }

  "sequence" should "convert a list of options into an option of list" in {
    val l1 = List(MySome(2), MySome(3), MySome(4))
    val l2 = List(MySome(1))
    val l3 = List(MySome(1), MyNone, MySome(5))

    MyOption.sequence(l1) shouldBe MySome(List(2, 3, 4))
    MyOption.sequence(l2) shouldBe MySome(List(1))
    MyOption.sequence(l3) shouldBe MyNone
  }

  "traverse" should "convert a list of options of A into an option of list of B" in {
    val l1 = List(MySome(2), MySome(3), MySome(4))
    val l2 = List(MySome(1))
    val l3 = List(MySome(1), MyNone, MySome(5))
    val l4 = List(MySome("Hello"), MySome("World"))

    MyOption.traverse(l1)(x => x.map(_ * 2)) shouldBe MySome(List(4, 6, 8))
    MyOption.traverse(l2)(x => x.map(_ * 2)) shouldBe MySome(List(2))
    MyOption.traverse(l3)(x => x.map(_ * 2)) shouldBe MyNone

    assertThrows[NumberFormatException] {
      MyOption.traverse(l4)(x => x.map(_.toInt)) shouldBe MyNone
    }
  }

  "sequenceAsTraverse" should "the same as sequence" in {
    val l1 = List(MySome(2), MySome(3), MySome(4))
    val l2 = List(MySome(1))
    val l3 = List(MySome(1), MyNone, MySome(5))

    MyOption.sequenceAsTraverse(l1) shouldBe MyOption.sequence(l1)
    MyOption.sequenceAsTraverse(l2) shouldBe MyOption.sequence(l2)
    MyOption.sequenceAsTraverse(l3) shouldBe MyOption.sequence(l3)
  }

}