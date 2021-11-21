object Anagram:
  def findAnagrams(word: String, potentialAnagrams: List[String]): List[String] =
    potentialAnagrams
      .filter(_.toLowerCase != word.toLowerCase)
      .filter(_.toLowerCase.sorted == word.toLowerCase.sorted)
