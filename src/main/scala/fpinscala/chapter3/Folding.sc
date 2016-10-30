import fpinscala.chapter3.datastructures._

val ints = List(5, 7, 8)
val doubles = List(10.0, 12.2, 3.6)

List.sum2(ints)
List.product2(doubles)

// 3.8
List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))

List.length(List(1,2,2))

List.foldLeft(List("a", "b", "c"), "")((x, y) => x + y)


// 3.11
def sum3(l: List[Int]) =
  List.foldLeft(l, 0)(_ + _)

sum3(List(1,2,3))

def product(l: List[Double]) = List.foldLeft(l, 1.0)(_ * _)

product(List(0.2, 0.4))

// 3.12
def reverse[A](l: List[A]): List[A] =
  List.foldLeft(l, Nil:List[A])((x, y) => List.append(Cons(y, Nil), x))

reverse(List(1, 2, 3))