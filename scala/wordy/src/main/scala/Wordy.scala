import scala.util.parsing.combinator._

object Wordy extends RegexParsers {

  val number: Parser[Int] = """(-?\d+)""".r ^^ { _.toInt }

  val op: Parser[String] = "plus" | "minus" | "multiplied by" | "divided by"

  val parser: Parser[Option[Int]] =
    "What is" ~ number ~ rep(op ~ number) ~ "?" ^^ { case _ ~ x ~ ops ~ _ =>
      ops.foldLeft(Option.apply[Int](x)) { case (Some(result), op ~ y) =>
        op match {
          case "plus"          => Some(result + y)
          case "minus"         => Some(result - y)
          case "multiplied by" => Some(result * y)
          case "divided by"    => if (y == 0) Option.empty[Int] else Some(result / y)
          case _               => None
        }
      }
    }

  def answer(sentence: String): Option[Int] =
    parseAll(parser, sentence) match {
      case Success(result, _) => result
      case _                  => None
    }
}
