package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(x, xs) => xs
      case y => y
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Cons(x, xs) => Cons(h, xs)
      case y => y
    }
  }

  // drop the first n elements of the list
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }
  }

  // continue to drop elements that satisfy a condition expressed in f
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if (f(h) == false) Cons(h, t)
        else dropWhile(t, f)
      }
    }
  }

  // Return l's all but last element
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(h: List[A], t: List[A]): List[A] = {
      t match {
        // our stop condition -> for the last element of singly linked lists the tail attr will always be Nil
        case Cons(_, Nil) => h
        case Cons(x, xs) => go(List.append[A](h, List(x)), xs)
      }
    }
    l match {
      case Nil => Nil
      case Cons(h, t) => go(List(h), t)
    }
  }

  // Alternative implementation that doesn't rely on append method
  def init2[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      // this case condition is the recursive function call's short circuit.
      // For lists of longer than 2, the pattern will always evaluate to Cons(h, t)
      // until we have iterated up to the last element of the list, where it will match this condition.
      // The last call will thus return a value (Nil), rather than calling the function again.
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // "Note that foldRight must traverse all the way to the end of the list (pushing frames
  // onto the call stack as it goes) before it can begin collapsing it"!
  // That's how we can add to y in f. y is an uninitialised object when foldRight is first
  // called recursively.
  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => y + 1)

  // This makes much more sense to me than foldRight. Here we can use z to initialize our
  // 'accumulator' with a value. We start collapsing right away. The z value we pass to our
  // second call to foldLeft will already be the result of f(z,x), let's say 0 + x if f is the
  // sum function.
  // Important takeaway point here: keep the order of operations in sight. We first execute the
  // operations *inside* the parantheses - in this case f(z,x) before anything else.
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  def reverse[A](l: List[A]) = foldLeft(l, Nil:List[A])((x, y) => Cons(y,x))

  def append2[A](l1: List[A], l2: List[A]): List[A] = {
    l2 match {
      case Nil => Nil
      case Cons(x, xs) => foldLeft(l1, Nil:List[A])((b,a) => append())
    }
    foldLeft(l1, Nil:List[A])((x, y) => Cons(y,x))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
