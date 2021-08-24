object DifferenceOfSquares {

  def square(n: Int): Int = n * n

  def sumOfSquares(n: Int): Int = (1 to n).map(square).sum

  def squareOfSum(n: Int): Int = square((1 to n).sum)

  def differenceOfSquares(n: Int): Int = squareOfSum(n) - sumOfSquares(n)

}
