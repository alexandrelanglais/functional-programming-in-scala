package basics

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MySome(x) => MySome(f(x))
    case MyNone    => MyNone
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MySome(x) => f(x)
    case MyNone    => MyNone
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MySome(x) => x
    case MyNone    => default
  }

  def orElse[B >: A](op: => MyOption[B]): MyOption[B] = this match {
    case MySome(x) => this
    case MyNone    => op
  }

  def filter(p: A => Boolean): MyOption[A] = this match {
    case MySome(x) if p(x) => this
    case _                 => MyNone
  }

}

case class MySome[A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object MyOption {
  def map2[A, B, C](op1: MyOption[A], op2: MyOption[B])(
      f: (A, B) => C): MyOption[C] = {
    op1.flatMap(o1 => op2.flatMap(o2 => MySome(f(o1, o2))))
  }

  def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] = as match {
    case x :: xs if x == MyNone => MyNone
    case x :: xs => map2(x, sequence(xs))((a, b) => a :: b)
    case Nil => MySome(Nil)
  }

  def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] = as match {
    case x :: xs if f(x) == MyNone => MyNone
    case x :: xs => map2(f(x), traverse(xs)(f))((a, b) => a :: b)
    case Nil => MySome(Nil)
  }

  def sequenceAsTraverse[A](as: List[MyOption[A]]): MyOption[List[A]] = {
    traverse(as)(x => x)
  }
}

object MyOptionAnnexes {
  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def insuranceRateQuote(a: Int, n: Int): Double = ???

  def parseInsuranceRateQuota(age: String,
                              nbOfSpeedingTickets: String): MyOption[Double] = {
    val a: MyOption[Int] = MyTry(age.toInt)
    val n: MyOption[Int] = MyTry(nbOfSpeedingTickets.toInt)

    MyOption.map2(a, n)(insuranceRateQuote)
  }

  def MyTry[A](v: => A): MyOption[A] = {
    try {
      MySome(v)
    } catch { case e: Exception => MyNone }
  }

}
