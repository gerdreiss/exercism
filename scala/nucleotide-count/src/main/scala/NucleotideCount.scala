class DNA(private val v: String) {

  val emptyResult: Either[String, Map[Char, Int]] = Right("ACGT".map(_ -> 0).toMap)

  /** my original solution */
  val nucleotideCounts: Either[String, Map[Char, Int]] =
    v.foldLeft(emptyResult) {
      case (Right(acc), ch) =>
        acc
          .get(ch)
          .toRight("invalid nucleotides string")
          .map(count => acc.updated(ch, count + 1))
      case (left, _)        => left
    }

  /** much more elegant solution */
  //lazy val nucleotideCounts = "ACGT".map(_ -> 0).toMap ++ v.groupBy(identity).mapValues(_.length)
}
