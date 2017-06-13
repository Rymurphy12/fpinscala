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

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_+_)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_*_)

  //Exercise 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new NoSuchElementException("Trying to get the tail of a Nil List")
    case Cons(x, xs) => xs
  }

  //Exercise 3.3
  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Nil => throw new NoSuchElementException("Trying to set a head on a Nil List")
    case Cons(x, xs) => Cons(head, xs)
  }

  //Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = (l,n) match {
    case (xs,0) => xs
    case (Nil, n) => Nil
    case (Cons(_, xs), n) => drop(xs, n -1)
  }

  //Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else l
  }

  //Exercise 3.6
  //The reason it can't be done in constant time is because the entire list
  //must be copied from frist to the second to last value this is inefficient
  //and could cause a stack overflow
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("Trying to get all but the last element on a Nil List")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  //Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_ ,acc) => acc + 1)

  //Exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs,f(x, z))(f)
  }

  //Exercise 3.11a
  def sumFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_+_)

  //Exercise 3.11b
  def productFoldLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_*_)

  //Exercise 3.11c
  def lengthFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((_, acc) => acc + 1)

  //Exercise 3.12
  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((x, acc) => Cons(x, acc))

  //Exercise 3.13
  def foldRightByFoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)(f)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


object ListDriver {
  def main(args: Array[String]): Unit = {
    //Exercise 3.1
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //This case will be the first pattern to be matched. The result will be 3
      case Cons(h,t) => h + List.sum(t)
    }

    //Exercise 3.8e
    //Assumption: This should produce the same list
    val xs = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  }
}
