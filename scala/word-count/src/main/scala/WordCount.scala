case class WordCount(string: String) {
  lazy val countWords: Map[String, Int] =
    string.toLowerCase
      .split("[^a-z0-9']")
      .map(s => if (isQuoted(s)) s.tail.init else s)
      .filterNot(_.isEmpty)
      .groupBy(identity)
      .mapValues(_.length)


  private def isQuoted(s: String): Boolean =
    s.startsWith("\'") && s.endsWith("\'") || s.startsWith("\"") && s.endsWith("\"")
}
