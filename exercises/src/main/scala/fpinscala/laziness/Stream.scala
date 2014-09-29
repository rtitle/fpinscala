package fpinscala.laziness

import Stream._
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
  
  // 5.1
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case Empty => Nil
  }

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
    case _ => Empty
  } 

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n-1)
    case otherwise@_ => otherwise
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  // 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else Empty)

  // 5.6
  def headOption: Option[A] = foldRight[Option[A]](None)((a,b) => Some(a))  // recursion doesn't continue because b isn't evaluated

  // 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else b)
  def append[B >: A](s2: => Stream[B]): Stream[B] = foldRight(s2)((a,b) => cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => f(a).append(b))

  // this wasn't in the book but I'll implement it anyway
  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(x,xs), Cons(y,ys)) => if (x() == y()) xs().startsWith(ys()) else false
    case (_, Empty) => true
    case _ => false
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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

  // 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // 5.10 
  def fibs: Stream[Int] = {
    def inner(cur: Int, prev: Int): Stream[Int] = cons(prev, inner(prev+cur, cur))
    inner(1, 0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
