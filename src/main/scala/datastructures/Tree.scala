package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A],right: Tree[A]) extends Tree[A]

object Tree {

  //Exercise 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  //Exercise 3.26
  def maximum[A](tree: Tree[A]): A = tree match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  //Exercise 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 +(depth(l) max depth(r))
  }

  //Exercise 3.28
  def map[A,B](tree: Tree[A])(f: A => B): List[B] = {
    case Leaf(n) => Leaf(f(n))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

}
