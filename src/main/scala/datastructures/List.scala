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
    
}
