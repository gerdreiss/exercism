import scala.util.parsing.combinator._

object Wordy extends RegexParsers {

  val number: Parser[Int] = """(-?\d+)""".r ^^ { _.toInt }

  val op: Parser[String] = "plus" | "minus" | "multiplied by" | "divided by" | "raised to the"

  val parser: Parser[Option[Int]] =
    "What is" ~ number ~ rep(op ~ number ~ """((st|nd|rd|th) power)?""".r) ~ "?" ^^ {
      case _ ~ x ~ ops ~ _ =>
        ops.foldLeft(Option[Int](x)) {
          case (Some(result), op ~ y ~ _) =>
            op match {
              case "plus" => Some(result + y)
              case "minus" => Some(result - y)
              case "multiplied by" => Some(result * y)
              case "divided by" => if (y == 0) None else Some(result / y)
              case "raised to the" => Some(math.pow(result, y).toInt)
              case _ => None
            }
        }
    }

  def answer(sentence: String): Option[Int] =
    parseAll(parser, sentence) match {
      case Success(result, _) => result
      case _ => None
    }
}
