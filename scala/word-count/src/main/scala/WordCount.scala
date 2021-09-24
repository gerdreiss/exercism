case class WordCount(string: String) {

  lazy val countWords: Map[String, Int] =
    string.toLowerCase
      .split("[^a-z0-9']")
      .map(unquote)
      .filterNot(_.isEmpty)
      .groupBy(identity)
      .mapValues(_.length)

  private def unquote(s: String): String =
    if (s.startsWith("\'") && s.endsWith("\'") || s.startsWith("\"") && s.endsWith("\"")) s.tail.init else s

}
