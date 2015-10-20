package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def getOrElse[B>:A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = sys.error("todo")

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case None => None
      case x if !f(x.getValue) => None
      case _ => this
    }
  }

  def getValue: A = {
    this match {
      case None => throw new IllegalArgumentException("the arg was wrong...");
      case Some(a) => a
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap( aa => b.map( bb => f(aa,bb)))

  def sequence_imperative[A](a: List[Option[A]]): Option[List[A]] = {
    val nones = a.find((item) => item == None)
    if (!nones.isEmpty) { return None }
    val list = a.map((item:Option[A]) => item.getValue)
    Some(list)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft[Option[List[A]]](Some(Nil))((acc,value) =>
      map2(acc,value)( (acc, value) => value :: acc ))
  }

  def flatten[A](a: List[Option[A]]): List[A] = {
    a match {
      case Nil => Nil
      case None :: tail => flatten(tail)
      case Some(head) :: tail => head :: flatten(tail)
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}
