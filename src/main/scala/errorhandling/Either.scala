package fpinscala.exceptions

sealed trait Either[+E, +A]{

  def Try(a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  //Exercuse 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  //Exercuse 4.6
  def flatmap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  //Exercuse 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  //Exercuse 4.6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatmap (x => b map(y => f(x,y)))

  //Exercise 4.7
  def sequence(es: List[Either[E,A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => h flatmap(x => sequence(t) map(y => x :: y))
  }

  //Exercise 4.7
  def traverse[B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] =
    as.foldRight[Either[E,List[B]]](Right(Nil))((x,acc) => f(x).map2(acc)(_::_))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
