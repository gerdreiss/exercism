object Sieve {

  // my solution
  def primes(n: Int): List[Int] = {
    def multiples(x: Int): List[Int] =
      (x to n).sliding(x, x).map(_.head).toList.tail

    (2 to n).diff((2 to n).flatMap(multiples)).toList
  }

  // somebody else's solution - very elegant
  def primes2(n: Int): List[Int] =
    (2 to n)
      .flatMap(x => x to n by x)
      .groupBy(identity)
      .filter(_._2.length == 1)
      .keys
      .toList
      .sorted

}
