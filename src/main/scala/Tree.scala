sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size_1[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum_1(t: Tree[Int]): Int = t match {
    case Leaf(x)      => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth_1[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(r) max depth(l))
  }

  def map_1[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x)      => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def size[A](t: Tree[A]): Int = {
    fold(t, (x: A) => 1) { (l, r) =>
      1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    fold(t, (x: Int) => x) { (l, r) =>
      maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t, (x: A) => 0) { (l, r) =>
      1 + (depth(r) max depth(l))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t, (x:A) => Leaf(f(x)): Tree[B]) { (l, r) =>
      Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t: Tree[A], z: (A => B))(f: (Tree[A], Tree[A]) => B): B =
    t match {
      case Leaf(x)      => z(x)
      case Branch(l, r) => f(l, r)
    }
}
