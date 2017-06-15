sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }
  def flatmap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    map(x => f(x))
  def orElse[EE >: E, B](b: => Either[EE,B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatmap (x => b map())
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
