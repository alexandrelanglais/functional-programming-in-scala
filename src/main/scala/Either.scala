sealed trait MyEither[+E, +A] {

  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyRight(x) => MyRight(f(x))
    case MyLeft(x) => MyLeft(x)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyRight(x) => f(x)
    case MyLeft(x) => MyLeft(x)
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(x) => b
    case _ => this
  }


  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
    for {
      x <- this
      y <- b
    } yield f(x, y)
  }

}

case class MyLeft[E](value: E) extends MyEither[E, Nothing]
case class MyRight[A](value: A) extends MyEither[Nothing, A]

object MyEither {

  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = es match {
    case MyLeft(x) :: _ => MyLeft(x)
    case MyRight(x) :: xs => MyRight(x).map2(sequence(xs))(_ :: _)
    case Nil => MyRight(Nil)
  }

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = as match {
    case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
    case Nil => MyRight(Nil)
  }
}

object MyEitherAnnex {
  def mean(xs: IndexedSeq[Double]): MyEither[String, Double] =
    if (xs.isEmpty)
      MyLeft("mean of empty list!")
    else
      MyRight(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): MyEither[Exception, Int] =
    try MyRight(x / y)
    catch { case e: Exception => MyLeft(e) }
}
