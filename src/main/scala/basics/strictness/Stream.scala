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

  def mapU[B](f: A => B): Stream[B] = {
    Stream.unfold(this) { s =>
      s match {
        case Empty => None
        case Cons(x, xs) => Some((f(x()), xs()))
      }
    }
  }

  def takeU(n: Int): Stream[A] = {
    Stream.unfold((this, 0)) { s =>
      val (str, cnt) = s
      str match {
        case Empty => None
        case Cons(x, xs) =>
          if (cnt < n) Some((x(), (xs(), cnt + 1))) else None
      }

    }
  }

  def takeWhileU(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) { s =>
      s match {
        case Cons(x, xs) if p(x()) => Some((x(), xs()))
        case _ => None
      }
    }
  }

  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, stream)) { s =>
      val (s1, s2) = s
      s1 match {
        case Empty => None
        case Cons(x, xs) =>
          s2 match {
            case Empty => None
            case Cons(xx, xxs) => Some((f(x(), xx()), (xs(), xxs())))
          }
      }
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(
    f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) =>
        Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) =>
        Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }
  }

  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Empty
  }

  def constantU[A](a: A): Stream[A] = {
    unfold(a)(s => Some(s, s))
  }

  def fromU(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s + 1))
  }

  def fibsU(): Stream[Int] = {
    unfold((0, 1))(s => Some(s._2, (s._2, s._1 + s._2)))
  }

  def onesU(): Stream[Int] = {
    unfold(1)(s => Some(1, 1))
  }

}
