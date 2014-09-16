package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.1: 3

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(x,xs) => xs
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_,_) => Cons(h, l)
  }

  // 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => l
      case Cons(x,xs) => drop(xs, n-1) 
    }
  }

  // 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }
    
  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(x, Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }

  // 3.7: It can't because it recurses to the end before it calls the folding function.

  // 3.8: should get List(1,2,3)

  // 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((a,b) => b+1)

  // 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
  }

  // 3.11
  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productLeft(l: List[Double]): Double = foldLeft(l, 1D)(_ * _)
  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((b,a) => b+1)

  // 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b,a) => Cons(a,b))

  // 3.13
  // tricky, had to look this one up
  def foldLeft2[A, B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, ((b: B) => b))((a,g) => b => g(f(b,a)))(z)
  def foldRight2[A, B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, ((b: B) => b))((g,a) => b => g(f(a,b)))(z)

  // 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, as) => Cons(a,as))

  // 3.15
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((as, b) => append2(as, b))  

  // 3.16
  def add1(l: List[Int]) = foldRight(l, Nil: List[Int])((a,b) => Cons(a+1, b))

  // 3.17
  def dtostr(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a,b) => Cons(a.toString, b))
 
  // 3.18 
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a,b) => Cons(f(a), b))

  // 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((a,b) => if (f(a)) Cons(a,b) else b)

  // 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  // 3.21 
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def zipAdd(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Cons(x,xs), Cons(y,ys)) => Cons(x+y, zipAdd(xs, ys))
    case _ => Nil
  }

  // 3.23
  def zipWith[A,B,C](a1: List[A], a2: List[B])(f: (A,B) => C): List[C] = (a1,a2) match {
    case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
    case _ => Nil
  }
 
  // 3.24
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(x,xs) => if (foldLeft(zipWith(sup,sub)(_==_), true)(_&&_)) true else hasSubsequence(xs, sub)
  }

}
