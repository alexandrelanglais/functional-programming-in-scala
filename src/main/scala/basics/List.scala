package basics

import scala.annotation.tailrec

sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class Cons[+A](h: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil       => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: MyList[Double]): Double = ints match {
    case MyNil        => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def foldRight[A, B](list: MyList[A], z: B)(f: (A, B) => B): B = list match {
    case MyNil       => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A, B](list: MyList[A], z: B)(f: (B, A) => B): B = list match {
    case MyNil       => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def apply[A](elems: A*): MyList[A] = {
    if (elems.isEmpty) MyNil
    else Cons[A](elems.head, apply(elems.tail: _*))
  }

  def tail[A](list: MyList[A]): MyList[A] = list match {
    case Cons(_, xs) => xs
    case MyNil       => MyNil
  }

  def setHead[A](list: MyList[A], newHead: A): MyList[A] = Cons(newHead, list)

  @tailrec
  def drop[A](list: MyList[A], n: Int): MyList[A] = {
    if (n == 0) list
    else
      list match {
        case MyNil      => MyNil
        case Cons(x, _) => drop(tail(list), n - 1)
      }
  }

  def dropWhile[A](list: MyList[A])(p: A => Boolean): MyList[A] = list match {
    case MyNil      => MyNil
    case Cons(x, _) => if (p(x)) dropWhile(tail(list))(p) else list
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case MyNil          => MyNil
    case Cons(h, MyNil) => MyNil
    case Cons(h, xs)    => Cons(h, init(xs))
  }

  def length[A](as: MyList[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  def sumL(l: MyList[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def productL(l: MyList[Double]): Double = {
    foldLeft(l, 1.0)(_ * _)
  }

  def lengthL[A](l: MyList[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  def reverse[A](l: MyList[A]): MyList[A] = {
    foldLeft(l, MyNil: MyList[A])((b, a) => Cons(a, b))
  }

  def foldRightUsingFoldLeft[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, z)((b, a) => f(a, b))
  }

  def append[A](as: MyList[A], e: A): MyList[A] = {
    foldLeft(reverse(as), Cons(e, MyNil))((b, a) => Cons(a, b))
  }

  def appendList[A](l: MyList[A], r: MyList[A]): MyList[A] =
    foldRight(l, r)(Cons(_,_))

  def concat[A](as: MyList[MyList[A]]): MyList[A] = {
    foldRight(as, MyNil: MyList[A])(appendList)
  }

  def addOne(as: MyList[Int]): MyList[Int] = {
    foldLeft(reverse(as), MyNil:MyList[Int])((b, a) => Cons(a + 1, b))
  }

  def toString(as: MyList[Double]): String = {
    foldLeft(as, "")((acc, a) => acc + a.toInt.toString)
  }

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = {
    foldRight(as, MyNil:MyList[B])((a, b) => Cons(f(a), b))
  }

  def filter[A](as: MyList[A])(p: A => Boolean): MyList[A] = {
    foldRight(as, MyNil:MyList[A])((a, b) => if (p(a)) Cons(a, b) else b)
  }

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = {
    foldRight(as, MyNil:MyList[B])((a, b) => appendList(f(a), b))
  }

  def filterF[A](as: MyList[A])(p: A => Boolean): MyList[A] = {
    flatMap(as)(x => if(p(x)) MyList(x) else MyNil)
  }

  def sumLists(as: MyList[Int], bs: MyList[Int]): MyList[Int] = as match {
    case MyNil => MyNil
    case Cons(ax, axs) => bs match {
      case MyNil => MyNil
      case Cons(bx, bxs) => Cons(ax + bx, sumLists(axs, bxs))
    }
  }

  def zipWith[A](as: MyList[A], bs: MyList[A])(op: (A, A) => A) : MyList[A] = as match {
    case MyNil => MyNil
    case Cons(ax, axs) => bs match {
      case MyNil => MyNil
      case Cons(bx, bxs) => Cons(op(ax, bx), zipWith(axs, bxs)(op))
    }
  }

  def take[A](as: MyList[A], n:Int): MyList[A] = {
    reverse(drop(reverse(as), length(as) - n))
  }

  def hasSubsequence[A](as: MyList[A], sub:MyList[A]): Boolean = take(as, length(sub)) match {
    case MyNil => false
    case `sub` => true
    case _ => hasSubsequence(tail(as), sub)
  }
}

object MyApp extends App {
  val x = MyList(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case MyNil                                 => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + MyList.sum(t)
    case _                                     => 101
  }
  println(x)

  val value: Int = 0
  val l = MyList.foldRight(MyList(1, 2, 3), MyNil: MyList[Int])(Cons(_, _))
  println(l)
}
