class DNA(private val v: String) {

  val emptyResult: Either[String, Map[Char, Int]] = Right("ACGT".map(ch => ch -> 0).toMap)

  def nucleotideCounts: Either[String, Map[Char, Int]] =
    v.foldLeft(emptyResult) {
      case (Right(acc), ch) =>
        acc
          .get(ch)
          .toRight("invalid nucleotides string")
          .map(count => acc.updated(ch, count + 1))
      case (left, _)        => left
    }
}
