// use reduce if return type is same as elements
val l1 = List(1,2,3,4,5)
l1.reduceRight((a,b) => a + b)

// use fold if return type is different from elements
class Transaction(val amount: Double) {}

val l2 = List(
  new Transaction(20.00),
  new Transaction(45.00),
  new Transaction(5.00),
  new Transaction(1.00)
)

class Total(val amount: Double) {}


val t = l2.foldRight(new Total(0.0))((a, t) => new Total(a.amount + t.amount))
t.amount
