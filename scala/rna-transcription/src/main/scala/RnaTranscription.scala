object RnaTranscription {

  val mapping = Map(
    'G' -> 'C',
    'C' -> 'G',
    'T' -> 'A',
    'A' -> 'U'
  )

  def toRna(dna: String): Option[String] =
    // solution 1
    //dna.foldLeft(Option("")) {
    //  case (Some(dna), nucleotide) => mapping.get(nucleotide).map(dna :+ _)
    //  case _                       => None
    //}
    // solution 2, maybe a little more readable
    dna.foldLeft(Option("")) { (maybeRna, nucleotide) =>
      for {
        rna    <- maybeRna
        mapped <- mapping.get(nucleotide)
      } yield rna :+ mapped
    }

}
