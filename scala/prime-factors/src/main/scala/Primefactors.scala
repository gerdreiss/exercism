import scala.annotation.tailrec

object PrimeFactors {

  def primes(numbers: Stream[Long]): Stream[Long] =
    numbers.head #:: primes(numbers.tail.filter(_ % numbers.head != 0))

  def factors(num: Long): List[Long] = {
    @tailrec
    def recurse(n: Long, primes: Stream[Long], factors: List[Long]): List[Long] =
      if (n == 1) factors.reverse
      else {
        val factor = primes.head
        if (n % factor == 0) recurse(n / factor, primes, factor :: factors)
        else recurse(n, primes.tail, factors)
      }

    recurse(num, primes(Stream.from(2).map(_.toLong)), List.empty)
  }

  def main(args: Array[String]): Unit = {
    println(primes(Stream.from(2).map(_.toLong)).take(100).mkString(","))
  }
}
