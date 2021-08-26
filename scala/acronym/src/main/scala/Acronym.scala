object Acronym {
  def abbreviate(phrase: String): String =
  phrase.toUpperCase.split("[^\\w']+").map(_.head).mkString
}
