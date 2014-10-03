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

  // 5.13
  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h,t) => Some(f(h()), t())
    case _ => None
  }
  def take2(n: Int): Stream[A] = unfold(n,this) {
    case (nn, Cons(h,t)) if nn > 0 => Some(h(), (nn-1, t()))
    case _ => None
  }
  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if p(h()) => Some(h(), t())
    case _ => None
  }
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold(this,s2) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this,s2) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1,t1), Empty) => Some((Some(h1()), None), (t1(), empty))
    case (Empty, Cons(h2,t2)) => Some((None, Some(h2())), (empty, t2()))
    case _ => None
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

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f(_)))
    case None => empty
  }

  // 5.12
  def ones2: Stream[Int] = unfold(1)(Some(1, _))
  def constant2[A](a: A): Stream[A] = unfold(a)(Some(a, _))
  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))
  def fibs2: Stream[Int] = unfold(1,0)(s => Some(s._2, (s._1+s._2, s._1)))

}
