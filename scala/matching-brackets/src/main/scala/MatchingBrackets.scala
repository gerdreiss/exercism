import scala.annotation.tailrec

object MatchingBrackets {

  private val pairs = "[]{}()<>"

  @tailrec
  def recurse(input: String): Boolean =
    pairs.sliding(2, 2).foldLeft(input)((remainder, pair) => remainder.replace(pair, "")) match {
      case ""              => true
      case r if r == input => false
      case r               => recurse(r)
    }

  def isPaired(input: String): Boolean =
    input.isEmpty || recurse(input.filter(c => pairs.contains(c)))

}
