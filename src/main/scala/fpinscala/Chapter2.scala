object Chapter2 {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    def loop(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else loop(n - 1, acc * n)
    loop(n, 1)
  }

  // 2-1
  def fibonacci(n: Int): Int = {
    def loop(a: Int, b: Int, n: Int): Int = {
      if (n <= 0) b
      else loop(b, a + b, n - 1)
    }
    loop(0, 1, n)
  }

  // monomorphic version of findFirst, specific to Array[String]
  def findFirstMono(ss: Array[String], key: String): Int = {
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  // polymorphic version of findFirst, works with all Arrays
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) - 1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }

  // 2-2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    loop(1)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)

  // 2-3
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // 2-4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2-5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    x => f(g(x))

  def main(args: Array[String]) = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println("The %dth fibonacci number is %d".format(7, fibonacci(7)))
  }
}
