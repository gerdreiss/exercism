object Series {
  def slices(length: Int, s: String): List[List[Int]] =
    s.sliding(length)
      .map(_.map(_.toString.toInt).toList)
      .toList
}
