package fpinscala.parallelism

import java.util.concurrent.{ Callable, ExecutorService, Future, TimeUnit }



/*Exercise 7.1
 def map2[A,B,C](par1: Par[A], par2: Par[B])(f: (A,B) => C): Par[C] */

object Par {
  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)


  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      a(es).get
    })

  //Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => unit(f(a))
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  //Exercise 7.5
  def sequence[A](a: List[Par[A]]): Par[List[A]] = a match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }
}
