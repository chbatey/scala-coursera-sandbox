def mapReduce(reduce: (Int, Int) => Int, unit: Int)(map: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) {
    unit
  } else {
    reduce(map(a), mapReduce(reduce, unit)(map)(a + 1, b))
  }
}


def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce((x, y) => x * y, 1)(f)(a, b)

def factorial(to: Int) = product(x => x)(1, to)


product(x => x * x)(3, 4)
factorial(5)
