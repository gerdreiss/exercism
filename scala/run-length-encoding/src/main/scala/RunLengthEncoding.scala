object RunLengthEncoding {

  def encode(input: String): String =
    input
      .foldLeft(Vector.empty[StringBuilder]) { (acc, c) =>
        if (acc.isEmpty || !acc.last.contains(c)) {
          acc :+ StringBuilder.newBuilder.append(c)
        } else {
          acc.last.append(c)
          acc
        }
      }
      .map { builder =>
        if (builder.length > 1) s"${builder.length}${builder.head}"
        else s"$builder"
      }
      .mkString

  def decode(input: String): String =
    input
      .foldLeft(Vector.empty[StringBuilder]) { (acc, c) =>
        if (acc.isEmpty || acc.last.exists(_.isDigit == false)) {
          acc :+ StringBuilder.newBuilder.append(c)
        } else {
          acc.last.append(c)
          acc
        }
      }
      .map(_.toString())
      .map { s =>
        if (s.head.isDigit) s.last.toString * s.takeWhile(_.isDigit).toInt
        else s
      }
      .mkString

}
