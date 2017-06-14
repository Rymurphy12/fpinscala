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
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  //Exercise 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 +(depth(l) max depth(r))
  }

  //Exercise 3.28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match{
    case Leaf(n) => Leaf(f(n))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  //Exercise 3.29
  //Utilized the hint located at
  //https://github.com/fpinscala/fpinscala/blob/master/answerkey/datastructures/29.hint.txt
  //to solve fold. All other solutions except mapByFold solved indepdently.
  //I did not realize that the type annotation had to be specified to work for base case.
  def fold[A,B](tree: Tree[A])(f: A => B)(g:(B,B) => B): B = tree match {
    case Leaf(n) => f(n)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeByFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((x,y) => 1 + x + y)

  def maxByFold(tree: Tree[Int]): Int =
    fold(tree)(x => x)((_ max _))

  def depthByFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((x,y) => (1 +(x max y)))

  def mapByFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
}
