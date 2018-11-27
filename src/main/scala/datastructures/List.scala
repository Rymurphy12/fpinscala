package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  /* 
   Exercise 3.1
   What will be the result of the following match expression?

   val x = List(1,2,3,4,5) match {
     case Cons(x, Cons(2, Cons(4, _))) => x
     case Nil => 42
     case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
     case Cons(h, t) => h + sum(t)
     case _ => 101
   }

   Answer: The result will be that the pattern will match the third option
   with the result being the Int 3. case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
   It is the first pattern that is matched so that is the result it will produce.
   */

  // Exercise 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil        => sys.error("You cannot get the tail of an empty list!")
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](newHead: A, xs: List[A]): List[A] = xs match {
    case Nil        => sys.error("You cannot set the head of an empty list!")
    case Cons(_, t) => Cons(newHead, t)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <=0)
      l
    else l match {
      case Nil        => Nil
      case Cons(_, t) => drop(t, n -1)
    }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil                => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Cons(_, t)         => t
  }

  //Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(xs: List[A], acc: List[A]): List[A] = xs match {
      case Cons(_, Nil) => acc
      case Cons(h, t)   => loop(t, Cons(h, acc))
      case Nil => sys.error("You cannot call init on an empty list")
    }
    loop(l, Nil)
  }

  def foldRight[A,B](as: List[A], z :B)(f: (A,B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_+_)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_*_)

  /*Exercise 3.7 This is not possiible. Because of foldRight's generic
    nature we must evaluate every argument on the list until we reach
    Nil  */

  //Exercise 3.8
  //Prediction: It will returnthe same list
  //Result: This is what happens. I think it shows that foldRight
  //        follows the conventions of the List by moving from left
  //        to right

  // Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil          => z
      case Cons(x, xs)  => foldLeft(xs, f(z, x))(f)
    }

  // Exercise 3.11
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  //Exercise 3.12
  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((y, x) => Cons(x, y))

  // Exercise 3.13
  def foldRight2[A,B](as: List[A], z :B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)((y, x) => f(x, y))

  // Exercise 3.14
  def append2[A](xs: List[A], ys: List[A]): List[A] =
    foldRight2(xs, ys)(Cons(_, _))

  // Exercise 3.15
  def concat[A](xxs: List[List[A]]): List[A] =
    foldRight2(xxs, Nil: List[A])(append(_,_))

  // Exercise 3.16
  def incrementList(xs: List[Int]): List[Int] =
    foldRight2(xs, Nil: List[Int])((x, y) => Cons(x +1, y))

  // Exercise 3.17
  def listDoubleToListString(xs: List[Double]): List[String] =
    foldRight2(xs, Nil: List[String])((x, y) => Cons(x.toString, y))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight2(as, Nil: List[B])((h, t) => Cons(f(h), t))

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil: List[A])((h, t) => if(f(h)) Cons(h,t) else t)

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if(f(x)) List(x) else Nil)
  
  // Exercise 3.22
  def addLists(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  }

  //Exercise 3.23
  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  //Exercise 3.24
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1,t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }
  


}