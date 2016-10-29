import fpinscala.chapter3.datastructures._

List(1, 2, 3) match {
  case _ => 42
}

List(4, 2, 3) match {
  case Cons(h, _) => h
}

val times = 15

times match {
  case 1 => "one"
  case 2 => "two"
  case _ => "some other number"
}

times match {
  case i if i == 1 => "one"
  case i if i == 2 => "two"
  case _ => "some other number"
}

def bigger(o: Any): Any = o match {
  case i: Int if i < 0 => i - 1
  case i: Int if i > 0 => i + 1
  case d: Double if d < 0.0 => d - 0.1
  case d: Double if d > 0.0 => d + 0.1
  case text: String => text + "s"
}

bigger(0.4)
bigger(-0.1)
bigger(-23)
bigger(2003)
bigger("car")

case class Calculator(brand: String, model: String)

val hp20b = Calculator("HP", "20B")
val hp30b = Calculator("HP", "30B")
val hp48g = Calculator("HP", "48G")

hp20b.toString

def calcType(calc: Calculator) = calc match {
  case Calculator("HP", "20B") => "financial"
  case Calculator("HP", "48G") => "scientific"
  case Calculator("HP", "30B") => "business"
  case Calculator(brand, model) => "Calculator: %s %s is of unknown type.".format(brand, model)
}

calcType(hp20b)
calcType(hp30b)
calcType(hp48g)
calcType(Calculator("Texas Instruments", "TI-89"))

// 3.1
val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

List.tail(List(1,2,3))

// 3.3
