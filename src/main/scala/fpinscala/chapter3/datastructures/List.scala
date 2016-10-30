package fpinscala.chapter3.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case l => l
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case l => l
  }

  def betterDropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => betterDropWhile(t)(f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def append[A](a1: List[A], a: A): List[A] = a1 match {
    case Nil => Cons(a, Nil)
    case Cons(h, t) => Cons(h, append(t, Cons(a, Nil)))
  }

  // 3.6
  def init[A](l: List[A]): List[A] = {
    def loop[A](acc: List[A], xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, Nil) => acc
      case Cons(h, t) => loop(List.append(acc, Cons(h, Nil)), t)
    }
    loop(Nil, l)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // 3.7
  /*
    It isn't possible to halt the recursion of foldRight as it is currently implemented.
    Introducing a base case would break the generalization of the function.
   */

  // 3.9
  def length[A](l: List[A]): Int = List.foldRight(l, 0)((_, z) => z + 1)

  // 3.14
  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a, as) => Cons(a, as))

  // 3.16
  def addOne(l: List[Int]): List[Int] =
    List.foldLeft(l, Nil: List[Int])((xs, x) =>
      List.append(xs, Cons(x + 1, Nil)))

  // 3.17
  def doublesToStrings(l: List[Double]): List[String] =
    List.foldLeft(l, Nil: List[String])((xs, x) =>
      List.append(xs, List(x.toString)))

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    List.foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  // 3.19
  def filter[A](l: List[A], f: A => Boolean): List[A] =
    List.foldRight(l, Nil: List[A])((h: A, t: List[A]) => if (f(h)) Cons(h, t) else t)

  // 3.20
//  def flatMap[A,B](as: List[A])(f: A=> List[B]): List[B] =


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
