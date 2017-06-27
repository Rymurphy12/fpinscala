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
  def append[B >: A](a2: => Stream[B]): Stream[B] =
    foldRight(a2)(Stream.cons(_,_))

  //Exercise 5.7
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => f(h).append(t))

  //Exercise 5.8 is moved to the companion object

  //Exercise 5.9
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  //Exercise 5.10
  def fibs(): Stream[Int] = {
    def go(first: Int, second: Int): Stream[Int] = {
      Stream.cons(first, go(second, first + second))
    }
    go(0,1)
  }
  //Exercise 5.11
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case Some((a,s)) => Stream.cons(a,unfold(s)(f))
    case None => Stream.empty
  }

  //Exercise 5.12
  def fibsUnfold(): Stream[Int] = {
    unfold((0,1)){ case (x, y) => Some((x, (y, x + y)))}
  }

  //Exercise 5.12
  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n , n + 1)))

  //Exercise 5.12
  def constantUnfold(a: A): Stream[A] =
    unfold(a)(a => Some((a, a)))

  //Exercise 5.12
  def onesUnfold(): Stream[Int] =
    unfold(1)(_ => Some((1,1)))

  //Exercise 5.13
  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  //Exercise 5.13
  def takeUnfold(n:Int): Stream[A] =
    unfold((this, n)){
      case (Cons(h,_), 0) => Some((h(),(Stream.empty,0)))
      case (Cons(h,t), n) => Some((h(), (t(), n -1)))
      case _ => None
    }

  //Exercise 5.13
  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h,t) if p(h()) => Some((h(),t()))
      case _ => None
    }

  //Exercise 5.13
  def zipWidth(s2: Stream[A])(f:(A,A) => A): Stream[A] =
    unfold((this, s2)){
      case (Cons(h1,t1), Cons(h2,t2)) =>  Some((f(h1(),h2()), (t1(),t2())))
      case _ => None
    }

  //Exercise 5.13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    def go[C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h,t), Empty) => Some((f(Some(h()), None), (t(), Empty)))
        case (Empty, Cons(h,t)) => Some((f(None, Some(h())), (Empty, t())))
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      }
      go(s2)((_,_))
  }

  //Exercise 5.14
  def startsWith(s: Stream[A]): Boolean =
    zipAll(s).takeWhileUnfold(!_._2.isEmpty).forAll {
      case (h,h2) => h == h2
    }

    //Exercise 5.15
    def tails: Stream[Stream[A]] =
      unfold(this) {
        case Empty => None
        case s => Some((s,s drop 1))
      } append Stream(Stream.empty)

    def hasSubsequence(s: Stream[A]) : Boolean =
      tails exists (_ startsWith s)

    //Exercise 5.16
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a, p0) => {
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, Stream.cons(b2, p1._2))
      })._2
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

  //Exercise 5.8
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }
}
