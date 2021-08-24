object Pangrams {
  def isPangram(input: String): Boolean =
    input.toLowerCase.filter(_.isLetter).groupBy(identity).size == ('a' to 'z').size
}
