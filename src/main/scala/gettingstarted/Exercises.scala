package fpinscala.gettingstarted


object Exercises {

  // Exercise 2.1
  def fib(n: Int): Int = {
    def go(fst: Int, snd: Int, n: Int): Int =
      if (n == 0)
        fst
      else
        go(snd, fst + snd, n -1)
    go(0,1, n)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length -1)
        true
      else if (ordered(as(n), as(n+1)))
        loop(n + 1)
      else
        false
    loop(0)
  }
}
