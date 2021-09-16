import scala.annotation.tailrec

object PascalsTriangle {

  def rows(rows: Int): List[List[Int]] =
    // my original solution
    {
      @tailrec
      def helper(row: Int, acc: List[List[Int]]): List[List[Int]] =
        if (row > rows) acc
        else helper(row + 1, acc :+ ((1 :: acc.last.sliding(2).map(_.sum).toList) :+ 1))

      if (rows < 3) (1 to rows).map(row => List.fill(row)(1)).toList
      else helper(3, List(List(1), List(1, 1)))
    }
    // a much more elegant solution:
    // Stream.iterate(List(1))(prev => (0 +: prev) zip (prev :+ 0) map Function.tupled(_ + _)).take(rows).toList
}
