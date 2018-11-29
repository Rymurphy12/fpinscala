package fpinscala.errorhandling

sealed trait Option[+A]{
	// Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
  	case Some(x) => Some(f(x))
  	case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
  	this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
  	case Some(x) => x
  	case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = 
    this.map(x => Some(x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
  	this.flatMap(x => if(f(x)) Some(x) else None)
}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]