package fpinscala.state

trait RNG {

  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)

  //Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    i match {
      case n if n < 0 =>  (-(n + 1), rng2)
      case _ =>  (i, rng2)
    }
  }
  //Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = rng.nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  //Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i,d), rng2)
  }

  //Exercise 6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  //Exercise 6.3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  //Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, xs: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count == 0)
        (xs,rng)
      else{
        val (i, r) = rng.nextInt
        go(count - 1, i :: xs)(r)
      }
    }
    go(count, Nil)(rng)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng)
    }

  //Exercise 6.5
  def doubleViaMap(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  //Exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a,b), rng2)
    }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))

  //Exercise 6.7
  def sequence[A](fs: List[Rand[A]]):Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((x,y) => map2(x, y)(_ :: _))
}



case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
