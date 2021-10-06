import Expr.{Lit, Op}

import scala.util.parsing.combinator._

sealed trait OpType {
  def exec(n1: Int, n2: Int): Option[Int]
}

object OpType {

  case object Sum extends OpType {
    override def exec(n1: Int, n2: Int): Option[Int] = Some(n1 + n2)
  }

  case object Diff extends OpType {
    override def exec(n1: Int, n2: Int): Option[Int] = Some(n1 - n2)
  }

  case object Prod extends OpType {
    override def exec(n1: Int, n2: Int): Option[Int] = Some(n1 * n2)
  }

  case object Div extends OpType {
    override def exec(n1: Int, n2: Int): Option[Int] = if (n2 == 0) None else Some(n1 / n2)
  }

  case object Pow extends OpType {
    override def exec(n1: Int, n2: Int): Option[Int] = Some(math.pow(n1, n2).toInt)
  }

  def make(s: String): Option[OpType] = s match {
    case "plus"                => Some(Sum)
    case "minus"               => Some(Diff)
    case "multiplied by"       => Some(Prod)
    case "divided by"          => Some(Div)
    case "raised to nth power" => Some(Pow)
    case _                     => None
  }
}

sealed trait Expr
object Expr {

  final case class Lit(n: Int)                             extends Expr
  final case class Op(op: OpType, left: Expr, right: Expr) extends Expr

  def eval(expr: Expr): Option[Int] = expr match {
    case Lit(n)              => Some(n)
    case Op(op, left, right) =>
      for {
        l   <- eval(left)
        r   <- eval(right)
        res <- op.exec(l, r)
      } yield res
  }
}

object ExprLexer extends RegexParsers {
  val whatIs: Parser[String]       = """What is""".r
  val number: Parser[Int]          = """([-]?\d+)""".r ^^ { _.toInt }
  val plus: Parser[String]         = """plus""".r
  val minus: Parser[String]        = """minus""".r
  val multipliedBy: Parser[String] = """multiplied by""".r
  val dividedBy: Parser[String]    = """divided by""".r
  val raisedTo: Parser[String]     = """raised to the""".r
  val nthPower: Parser[String]     = """(st|nd|rd|th) power""".r

  val justNum: ExprLexer.Parser[Option[Expr]] = whatIs ~ number ^^ { case _ ~ n =>
    Some(Lit(n))
  }

  val arithmetic: ExprLexer.Parser[Option[Expr]] =
    whatIs ~ number ~ rep((plus | minus | multipliedBy | dividedBy) ~ number) ^^ { case _ ~ n1 ~ ops =>
      ops.foldLeft(Option.apply[Expr](Lit(n1))) { case (Some(expr), op ~ n) =>
        OpType.make(op).map(Op(_, expr, Lit(n)))
      }
    }

  val power: ExprLexer.Parser[Option[Expr]] =
    whatIs ~ number ~ raisedTo ~ number ~ nthPower ^^ { case _ ~ n1 ~ r ~ n2 ~ p =>
      OpType.make(r + " nth " + p).map(Op(_, Lit(n1), Lit(n2)))
    }

  val all: ExprLexer.Parser[Option[Expr]] = arithmetic | power | justNum
}

object Wordy {
  def answer(sentence: String): Option[Int] =
    ExprLexer.parse(ExprLexer.all, sentence).get.flatMap(Expr.eval)
}
