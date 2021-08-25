import scala.util.matching.Regex

object Series {
  def largestProduct(length: Int, source: String): Option[Int] =
    if (length == 0) Some(1)
    else if (length < 0 || length > source.length || !source.forall(_.isDigit)) None
    else Some(source.sliding(length).map(_.map(_.toString.toInt).product).max)
}