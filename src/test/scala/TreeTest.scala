import org.scalatest._

class TreeTest extends FlatSpec with Matchers {

  "Size" should "be able to retrieve the number of leaves and branches of a tree" in {
    val tree1 = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))
    val tree2 = Branch(Leaf(1), Leaf(2))

    Tree.size(tree1) shouldBe (7)
    Tree.size(tree2) shouldBe (3)
  }

  "Maximum" should "be able to retrieve the maximum value of a tree" in {
    val tree1 = Branch(Branch(Leaf(1), Branch(Leaf(8), Leaf(3))), Leaf(4))
    val tree2 = Branch(Leaf(1), Leaf(2))
    val tree3 = Branch(Leaf(8), Branch(Leaf(15), Leaf(3)))
    val tree4 = Branch(Branch(Leaf(25), Branch(Leaf(8), Leaf(3))), Leaf(4))

    Tree.maximum(tree1) shouldBe (8)
    Tree.maximum(tree2) shouldBe (2)
    Tree.maximum(tree3) shouldBe (15)
    Tree.maximum(tree4) shouldBe (25)
  }

  "Depth" should "be able to retrieve the maximum depth of a tree" in {
    val tree1 = Branch(Branch(Leaf(1), Branch(Leaf(8), Leaf(3))), Leaf(4))
    val tree2 = Branch(Leaf(1), Leaf(2))
    val tree3 = Branch(Leaf(8), Branch(Leaf(15), Leaf(3)))
    val tree4 = Branch(Leaf(8), Branch(Leaf(15), Branch(Branch(Leaf(4), Branch(Leaf(5), Leaf(5))), Leaf(5))))

    Tree.depth(tree1) shouldBe (3)
    Tree.depth(tree2) shouldBe (1)
    Tree.depth(tree3) shouldBe (2)
    Tree.depth(tree4) shouldBe (5)
  }

  "Map" should "be able to apply a function to every leaf of a tree" in {
    val tree1 = Branch(Branch(Leaf(1), Branch(Leaf(8), Leaf(3))), Leaf(4))

    Tree.map(tree1)(_ * 2) shouldBe (Branch(Branch(Leaf(2), Branch(Leaf(16), Leaf(6))), Leaf(8)))
  }
}