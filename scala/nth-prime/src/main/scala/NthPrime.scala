object NthPrime {

  def prime(n: Int): Option[Int] = {
    if (n == 0) None
    else primes().drop(n - 1).headOption
  }

  private def primes(numbers: Stream[Int] = Stream.from(2)): Stream[Int] =
    numbers.head #:: primes(numbers.tail.filter(_ % numbers.head != 0))

}
