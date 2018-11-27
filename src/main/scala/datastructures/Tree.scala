package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // Exercise 3.25
  def size[A](xs: Tree[A]): Int = xs match {
  	case Leaf(x) => 1
  	case Branch(l,r) => 1 + size(l) + size(r)
  }

  //Exercise 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
  	case Leaf(x) => x
  	case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
  	case Leaf(_) => 1
  	case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l ,r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29 
  def fold[A, B](tree: Tree[A])(n: A => B)(b: (B, B) => B): B = tree match {
  	case Leaf(x) => n(x)
  	case Branch(l ,r) => b(fold(l)(n)(b), fold(r)(n)(b))
  }

  def size2[A](tree: Tree[A]): Int =
  	fold(tree)(n => 1)((l,r) => 1 + l + r)

  def maximum2(tree: Tree[Int]): Int =
  	fold(tree)(n => n)((l,r) => l max r)

  def depth2[A](tree: Tree[A]): Int =
  	fold(tree)(n => 0)((l,r) => 1 + (l max r))

  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] =
  	fold(tree)(n => Leaf(f(n)): Tree[B])((l,r) => Branch(l ,r))
}