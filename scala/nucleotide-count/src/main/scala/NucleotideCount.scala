class DNA(private val dna: String) {

  /** my first solution */
  private val emptyResult: Either[String, Map[Char, Int]] = Right("ACGT".map(_ -> 0).toMap)

  lazy val nucleotideCounts0: Either[String, Map[Char, Int]] =
    dna.foldLeft(emptyResult) {
      case (Right(acc), ch) =>
        acc
          .get(ch)
          .toRight("invalid nucleotides string")
          .map(count => acc.updated(ch, count + 1))
      case (left, _)        => left
    }

  /** another way to solve the problem */
  private val nucleotides = "ACGT"

  lazy val nucleotideCounts: Either[String, Map[Char, Int]] = {
    val result = dna.groupBy(identity).mapValues(_.length)
    if (result.keySet.forall(nucleotides.contains(_))) Right(nucleotides.map(_ -> 0).toMap ++ result)
    else Left("invalid dna string")
  }
}
