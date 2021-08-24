object Isogram {
  def isIsogram(word: String): Boolean =
    word.count(_.isLetter) == word.toLowerCase().filter(_.isLetter).groupBy(identity).size
}