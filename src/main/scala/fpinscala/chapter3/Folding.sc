import fpinscala.chapter3.datastructures._

val ints = List(5, 7, 8)
val doubles = List(10.0, 12.2, 3.6)

List.sum2(ints)
List.product2(doubles)

// 3.8
List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))

// 3.9

