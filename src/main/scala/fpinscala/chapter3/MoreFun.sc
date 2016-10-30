import fpinscala.chapter3.datastructures._

List.addOne(List(1, 2, 3))
List.doublesToStrings(List(19.55, 20.0, 30.6))

val x = List.map(List(5, 10, 15))((x: Int) => x * 2)

List.filter(List(5, 10, 15), (x: Int) => x >= 10)

val left1 = Branch[String](Leaf("a"), Leaf("b"))
val right1 = Branch[String](Leaf("c"), Leaf("d"))
val t1 = Branch(left1, right1)

Tree.size(t1)
Tree.depth(t1)
Tree.map(t1)(_.toUpperCase)
Tree.mapViaFold(t1)(_ + "z")

val left2 = Branch[Int](Leaf(324), Leaf(1095))
val right2 = Branch[Int](Leaf(33), Leaf(56))
val t2 = Branch(left2, right2)

Tree.maximum(t2)
Tree.maximum(Tree.map(t2)(_ + 2))
