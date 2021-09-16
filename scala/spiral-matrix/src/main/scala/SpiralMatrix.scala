object SpiralMatrix {
  def spiralMatrix(side: Int): List[List[Int]] = {
    def helper(n: Int, x: Int, y: Int): List[List[Int]] =
      if (x == 0) List.empty
      else (n until n + y).toList :: helper(n + y, y, x - 1).reverse.transpose

    helper(1, side, side)
  }
}
