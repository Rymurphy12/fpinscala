package fpinscala.laziness

sealed trait Stream[+A]{
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def existsExplicit(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p((h())) || t().existsExplicit(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  //Exercise 5.1
  def toList: List[A] = {
    def go(acc: List[A], s: Stream[A]): List[A] = s match {
      case Empty => acc.reverse
      case Cons(h, t) => go(h() :: acc, t())
    }
    go(Nil, this)
  }

  //Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  //Exercise 5.2
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  //Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match{
    case Cons(h,t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  //Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h,t) => p(h) && t)

  //Exercise 5.5
  def takeWhileByFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty )

  //Exercise 5.6
  def headOptionByFoldRight: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  //Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))

  //Exercise 5.7
  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) => if(f(h)) Stream.cons(h,t) else t)

  //Exercise 5.7
  def append(a2: () => Stream[A]): Stream[A] =
    foldRight(a2(): Stream[A])(Stream.cons(_,_))

  //Exercise 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])()
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, tl: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
