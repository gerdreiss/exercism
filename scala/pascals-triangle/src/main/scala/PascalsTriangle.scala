import scala.annotation.tailrec

object PascalsTriangle {
  def rows(rows: Int): List[List[Int]] = {
    @tailrec
    def helper(row: Int, acc: List[List[Int]]): List[List[Int]] =
      if (row > rows) acc
      else helper(row + 1, acc :+ ((1 :: acc.last.sliding(2).map(_.sum).toList) :+ 1))

    if (rows < 3) (1 to rows).map(row => List.fill(row)(1)).toList
    else helper(3, List(List(1), List(1, 1)))
  }
}
