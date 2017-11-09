package basics.strictness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Cons(h, xs) => h() :: xs().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], i: Int): Stream[A] = s match {
      case Empty => Empty
      case Cons(h, xs) if (i < n) => Stream.cons(h(), go(xs(), i + 1))
      case Cons(h, xs) if (i >= n) => Empty
    }

    go(this, 0)
  }

  def drop(n: Int): Stream[A] = {
    def go(s: Stream[A], i: Int): Stream[A] = s match {
      case Empty => Empty
      case Cons(h, xs) if (i < n) => go(xs(), i + 1)
      case Cons(h, xs) => Stream.cons(h(), go(xs(), i + 1))
    }

    go(this, 0)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, xs) if (p(h())) => Stream.cons(h(), xs().takeWhile(p))
    case Cons(h, xs) => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) =>
      if (p(a)) Stream.cons(a, b) else Empty)
  }

  def headOptionWithFoldRight(): Option[A] = {
    foldRight(Option.empty[A])((a, b) => if (a != Empty) Some(a) else None)
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
  }

  def append[B >: A](e: => Stream[B]): Stream[B] = {
    foldRight(e)((a, b) => Stream.cons(a, b))
  }

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a) append b)
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  def fibs(): Stream[Int] = {
    def go(p: Int, n: Int): Stream[Int] = {
      Stream.cons(p, go(n, n + p))
    }

    go(0, 1)
  }

  //  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
  //    for {
  //      t <- f(z)
  //    } yield Stream.cons(z, unfold(t)(f))
  //  }
}
