package fpinscala.laziness
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, _) if n == 1 => Stream(h())
    case Cons(h, t) if n > 1 => Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) if n == 1 => t()
    case Cons(_, t) if n > 1 => t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => if(p(h())) Stream.cons( h(), t() takeWhile p ) else Empty
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = {
    this.foldRight(Stream[A]())(
      (el, acc) => if (p(el)) Stream.cons(el, acc) else Empty
    )
  }

  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true)((element, acc) => acc && p(element))
  }

  def headOption: Option[A] = {
    this.foldRight[Option[A]](None)((el, acc) => Some(el))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList:List[A] = this match {
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toList
  }

  def map[B](f:A => B):Stream[B] = {
    foldRight[Stream[B]](Empty)((el, acc) => Stream.cons(f(el), acc))
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def equals(other: Any): Boolean = other match {
    case Cons(h2, t2) => h() == h2() && t() == t2();
    case _ => false
  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
