package fpinscala.state

trait RNG {
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
    (i / Int.MaxValue.toDouble, rng2)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
