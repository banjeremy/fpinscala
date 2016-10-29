def sum(xs: Int*): Int = xs.sum

sum(1, 2, 3, 4, 5)

def str(as: String*): String =
  as.foldRight("")((acc, a) => acc.concat(a))

str("one ", "two ", "three ")
