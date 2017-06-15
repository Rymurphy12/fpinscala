package fpinscala.exceptions


sealed trait Option[+A]{

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(n) => Some(f(n))
    case None => None
  }
  //exercise 4.1
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  //exercise 4.1
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(n) => n
    case None => default
  }

  //exercise 4.1
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  //exercise 4.1
  def filter(f: A => Boolean): Option[A] =
    flatMap(x => if (f(x)) Some(x) else None)

  //Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => Math.pow(x - m, 2 ))))


}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  //Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (x => b map(y => f(x,y)))

  //Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => None
    case h :: t => h flatMap(x => sequence(t) map(y => x :: y))
  }

  //Exercise 4.5
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, acc) => map2(f(x), acc)(_ :: _) )

  def sequenceByTraverse[A](a: List[Option[A]]):Option[List[A]] =
    traverse(a)(x => x)
}
