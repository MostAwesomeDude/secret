def factorial(n :int) :int {
  if (n == 0) {
    return 1
  } else if (n > 0) {
    return n * factorial(n-1)
  } else {
    throw("invalid argument to factorial: "+n)
  }
}
