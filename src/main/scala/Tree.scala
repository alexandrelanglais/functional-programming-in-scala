sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    def go(t: Tree[A], n: Int): Int = t match {
      case Leaf(_)      => n + 1
      case Branch(l, r) => go(l, n) + go(r, n) + 1
    }
    go(tree, 0)
  }

  def maximum(tree: Tree[Int]): Int = {
    def go(t: Tree[Int], m: Int): Int = t match {
      case Leaf(x)      => x.max(m)
      case Branch(l, r) => go(l, go(r, m))
    }
    go(tree, 0)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(t: Tree[A], n: Int, m: Int): Int = t match {
      case Leaf(_)      => n.max(m)
      case Branch(l, r) => go(r, go(l, (n + 1), (n + 1).max(m)), (n + 1).max(m))
    }
    go(tree, 0, 0)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    def go(t: Tree[A]): Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(go(l), go(r))
    }
    go(tree)
  }
}
