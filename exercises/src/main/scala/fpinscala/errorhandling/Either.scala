package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  // 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb) 
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    es.foldRight[Either[E, List[A]]](Right(Nil))(_.map2(_)(_ :: _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((x,y) => f(x).map2(y)(_ :: _))

  def sequence2[E, A](es: List[Either[E,A]]): Either[E, List[A]] = 
    traverse(es)(identity)

}
