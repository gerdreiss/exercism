object RnaTranscription {

  val mapping = Map(
    'G' -> 'C',
    'C' -> 'G',
    'T' -> 'A',
    'A' -> 'U'
  )

  def toRna(dna: String): Option[String] =
    dna.foldLeft(Option("")) {
      case (Some(dna), nucleotide) => mapping.get(nucleotide).map(dna :+ _)
      case _                       => None
    }

}
