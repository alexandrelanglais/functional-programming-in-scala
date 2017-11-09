package basics

import org.scalatest._

class ListTest extends FlatSpec with Matchers {

  "MyList" should "be able to compute sum of ints" in {
    val list: MyList[Int] = MyList(1, 2, 3)

    MyList.sum(list) shouldBe 6
  }

  it should "be able to multiply every items in the list" in {
    val list: MyList[Double] = MyList(1, 2, 3)

    MyList.product(list) shouldBe (6.0)
  }

  it should "return 1 if Nil" in {
    val list: MyList[Double] = MyNil

    MyList.product(list) shouldBe (1.0)
  }

  it should "return 0 if one element is 0" in {
    val list: MyList[Double] = MyList(1, 2, 0, 4)

    MyList.product(list) shouldBe (0)
  }

  "Tail function" should "return last elements from normal list" in {
    val list: MyList[Int] = MyList(1, 2, 3)

    MyList.tail(list) shouldBe (MyList(2, 3))
  }

  it should "return nil on a Nil list" in {
    val list: MyList[Int] = MyNil

    MyList.tail(list) shouldBe (MyNil)
  }

  it should "return Nil on a 1 element list" in {
    val list: MyList[Int] = MyList(1)

    MyList.tail(list) shouldBe (MyNil)
  }

  "Sethead function" should "add an element in front of a list" in {
    val list: MyList[Int] = MyList(2, 3)

    MyList.setHead(list, 1) shouldBe (MyList(1, 2, 3))

  }

  it should "return a one element list if passed argument is nil" in {
    MyList.setHead(MyNil, 1) shouldBe (MyList(1))
  }
  it should "add an element in front of a one item list" in {
    MyList.setHead(MyList(2), 1) shouldBe (MyList(1, 2))
  }

  "Drop function" should "return last elements from normal list" in {
    val list: MyList[Int] = MyList(1, 2, 3, 4, 5)

    MyList.drop(list, 2) shouldBe (MyList(3, 4, 5))
  }

  it should "return nil on a Nil list" in {
    val list: MyList[Int] = MyNil

    MyList.drop(list, 2) shouldBe (MyNil)
  }

  it should "return nil on a n > list size" in {
    val list: MyList[Int] = MyList(1, 2)

    MyList.drop(list, 3) shouldBe (MyNil)
  }

  it should "return nil on a n == list size" in {
    val list: MyList[Int] = MyList(1, 2)

    MyList.drop(list, 2) shouldBe (MyNil)
  }

  "Drop while function" should "drop first even element of a list" in {
    val list: MyList[Int] = MyList(2, 4, 6, 7, 8)

    MyList.dropWhile(list)(x => x % 2 == 0) shouldBe (MyList(7, 8))
  }

  it should "return nil on a nil list" in {
    MyList.dropWhile(MyNil)((x:Int) => x % 2 == 0) shouldBe (MyNil)
  }

  it should "return nil on a all predicate found" in {
    MyList.dropWhile(MyList(1, 3, 5, 7))(x => x % 2 != 0) shouldBe (MyNil)
  }

  it should "return same list on a no predicate found" in {
    MyList.dropWhile(MyList(1, 3, 5, 7))(x => x % 2 == 0) shouldBe (MyList(1, 3, 5, 7))
  }

  "Init function" should "return all but last elements of a list" in {
    val list: MyList[Int] = MyList(2, 4, 6, 7, 8)

    MyList.init(list) shouldBe (MyList(2, 4, 6, 7))
  }

  it should "return nil on a one element list" in {
    MyList.init(MyList(1)) shouldBe (MyNil)

  }
  it should "return nil on a nil list" in {
    MyList.init(MyNil) shouldBe (MyNil)

  }
  it should "return first element on a two elements list" in {
    MyList.init(MyList(1, 2)) shouldBe (MyList(1))
  }

  "Fold right" should "apply add operation on every element of a list" in {
    val list: MyList[Int] = MyList(1, 2, 3, 4)

    MyList.foldRight(list, 0)(_ + _) shouldBe (10)

  }
  it should "apply multiply operation on every element of a list" in {
    val list: MyList[Int] = MyList(1, 2, 3, 4)

    MyList.foldRight(list, 1)(_ * _) shouldBe (24)

  }

  "Length using fold right" should "return the correct length of a list" in {
    MyList.length(MyList(1, 2, 3, 4, 5)) shouldBe (5)
    MyList.length(MyList(1, 2)) shouldBe (2)
    MyList.length(MyList(1)) shouldBe (1)
  }

  it should "return 0 on a nil list" in {
    MyList.length(MyNil) shouldBe (0)
  }

  "Fold left" should "apply add operation on every element of a list" in {
    val list: MyList[Int] = MyList(1, 2, 3, 4)

    MyList.foldLeft(list, 0)(_ + _) shouldBe (10)

  }
  it should "apply multiply operation on every element of a list" in {
    val list: MyList[Int] = MyList(1, 2, 3, 4)

    MyList.foldLeft(list, 1)(_ * _) shouldBe (24)

  }

  "Length using fold left" should "return the correct length of a list" in {
    MyList.lengthL(MyList(1, 2, 3, 4, 5)) shouldBe (5)
    MyList.lengthL(MyList(1, 2)) shouldBe (2)
    MyList.lengthL(MyList(1)) shouldBe (1)
  }

  it should "return 0 on a nil list" in {
    MyList.lengthL(MyNil) shouldBe (0)
  }

  "Sum using fold left" should "return the sum of the elements" in {
    MyList.sumL(MyList(1, 2, 3, 4, 5)) shouldBe (15)
  }

  "Product using fold left" should "return the product of the elements" in {
    MyList.productL(MyList(1, 2, 3, 4, 5)) shouldBe (120)
  }

  "Reverse using a fold" should "return the reversed list" in {
    MyList.reverse(MyList(1, 2, 3)) shouldBe (MyList(3, 2, 1))
    MyList.reverse(MyList(1, 2)) shouldBe (MyList(2, 1))
    MyList.reverse(MyList(1)) shouldBe (MyList(1))
    MyList.reverse(MyNil) shouldBe (MyNil)
  }

  "Fold right using fold left" should "be the same as foldright" in {
    val list: MyList[Int] = MyList(1, 2, 3, 4)
    MyList.foldRight(list, 0)(_ + _) shouldBe (MyList.foldRightUsingFoldLeft(list, 0)(_ + _))
  }

  "Append" should "append an element to a list" in {
    MyList.append(MyList(1, 2, 3, 4), 5) shouldBe (MyList(1, 2, 3, 4, 5))
    MyList.append(MyList(1), 2) shouldBe (MyList(1, 2))
    MyList.append(MyNil, 5) shouldBe (MyList(5))
  }

  "Concat" should "concatenate a list of lists into a single list" in {
    val l1 = MyList(1, 2, 3)
    val l2 = MyList(4, 5, 6)
    val l3 = MyList(7, 8, 9)
    val con = MyList(l1, l2, l3)

    val res = MyList.concat(con)

    res shouldBe (MyList(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "AddOne" should "add one to every element of the list" in {
    MyList.addOne(MyList(1, 2, 3)) shouldBe (MyList(2, 3, 4))
  }

  "ToString" should "return a string from a list of doubles" in {
    MyList.toString(MyList(1, 2, 3)) shouldBe ("123")
  }

  "Map" should "modify a list maintaining its structure" in {
    MyList.map(MyList(1, 2, 3))(_ * 2) shouldBe (MyList(2, 4, 6))
  }

  "Filter" should "filter elements of a list" in {
    MyList.filter(MyList(1, 2, 3, 4, 5, 6))(_ % 2 == 0) shouldBe (MyList(2, 4, 6))
    MyList.filter(MyList(1, 2, 3, 4, 5, 6))(_ < 4) shouldBe (MyList(1, 2, 3))
  }
  "Flatmap" should "Return a flat list" in {
    MyList.flatMap(MyList(1, 2, 3))(i => MyList(i, i)) shouldBe (MyList(1, 1, 2, 2, 3, 3))
  }

  "FilterF" should "filter elements of a list" in {
    MyList.filterF(MyList(1, 2, 3, 4, 5, 6))(_ % 2 == 0) shouldBe (MyList(2, 4, 6))
    MyList.filterF(MyList(1, 2, 3, 4, 5, 6))(_ < 4) shouldBe (MyList(1, 2, 3))
  }

  "SumLists" should "add values of two lists" in {
    val l1 = MyList(1, 2, 3)
    val l2 = MyList(4, 5, 6)

    MyList.sumLists(l1, l2) shouldBe(MyList(5, 7, 9))
  }

  "ZipWith" should "be able to perform various operation on two lists" in {
    val l1 = MyList(1, 2, 3)
    val l2 = MyList(4, 5, 6)

    MyList.zipWith(l1, l2)(_ + _) shouldBe(MyList(5, 7, 9))
    MyList.zipWith(l1, l2)(_ * _) shouldBe(MyList(4, 10, 18))

    val l3 = MyList("Hel", ",", "Wor")
    val l4 = MyList("lo", " ", "ld!")

    MyList.zipWith(l3, l4)(_ + _) shouldBe(MyList("Hello", ", ", "World!"))
  }

  "Take" should "take the n first elements of a list" in {
    val l1 = MyList(1, 2, 3, 4, 5, 6, 7, 8, 9)
    MyList.take(l1, 3) shouldBe (MyList(1, 2, 3))
  }

  "HasSubsequence" should "find a sublist in another list" in {
    val l1 = MyList(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val l2 = MyList(4, 5, 6)
    val l3 = MyList(7, 7, 9)
    val l4 = MyList(7, 8, 9)

    MyList.hasSubsequence(l1, l4) shouldBe (true)
    MyList.hasSubsequence(l1, l2) shouldBe (true)
    MyList.hasSubsequence(l1, l3) shouldBe (false)

  }
}