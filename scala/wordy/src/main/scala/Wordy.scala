import Expr.{BinaryOp, Lit}

import scala.util.parsing.combinator._

sealed trait Op {
  def exec(n: Int, ns: Int*): Option[Int]
}

object Op {
  // Binary ops
  case object Sum    extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some((n :: ns.toList).sum)
  }
  case object Diff   extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some(n - ns.toList.sum)
  }
  case object Prod   extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some((n :: ns.toList).product)
  }
  case object Div    extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = ns.toList.product match {
      case 0 => None
      case p => Some(n / p)
    }
  }
  case object Pow    extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some(math.pow(n, ns.toList.sum).round.toInt)
  }
  // Unary ops
  case object Square extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some(math.sqrt(n).round.toInt)
  }
  case object Abs    extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some(math.abs(n))
  }
  case object Neg    extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some(-1 * n)
  }
  case object Lg     extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some(math.log(n).round.toInt)
  }
  case object Ln     extends Op {
    override def exec(n: Int, ns: Int*): Option[Int] = Some(math.log10(n).round.toInt)
  }

  def make(s: String): Option[Op] = s match {
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
  final case object Q                                        extends Expr
  final case class Lit(n: Int)                               extends Expr
  final case class UnaryOp(op: Op, n: Expr)                  extends Expr
  final case class BinaryOp(left: Expr, op: Op, right: Expr) extends Expr

  def eval(expr: Expr): Option[Int] = expr match {
    case Lit(n)                    => Some(n)
    case UnaryOp(op, ex)           => eval(ex).flatMap(n => op.exec(n))
    case BinaryOp(left, op, right) =>
      for {
        l   <- eval(left)
        r   <- eval(right)
        res <- op.exec(l, r)
      } yield res
  }
}

object ExprLexer extends RegexParsers {
  val whatIs: Parser[String] = """What is""".r

  val number: Parser[Int] = """([-]?\d+)""".r ^^ { _.toInt }

  val plus: Parser[String]         = """plus""".r
  val minus: Parser[String]        = """minus""".r
  val multipliedBy: Parser[String] = """multiplied by""".r
  val dividedBy: Parser[String]    = """divided by""".r

  val raisedTo: Parser[String] = """raised to the""".r
  val nthPower: Parser[String] = """(st|nd|rd|th) power""".r

  def justNum: ExprLexer.Parser[Option[Expr]] = whatIs ~ number ^^ { case _ ~ n =>
    Some(Lit(n))
  }

  def arithmetic: ExprLexer.Parser[Option[Expr]] =
    whatIs ~ number ~ rep((plus | minus | multipliedBy | dividedBy) ~ number) ^^ { case _ ~ n1 ~ ops =>
      ops.foldLeft(Option.apply[Expr](Lit(n1))) { case (Some(expr), op ~ n) =>
        Op.make(op).map(BinaryOp(expr, _, Lit(n)))
      }
    }

  def power: ExprLexer.Parser[Option[Expr]] =
    whatIs ~ number ~ raisedTo ~ number ~ nthPower ^^ { case _ ~ n1 ~ r ~ n2 ~ p =>
      Op.make(r + " nth " + p).map(BinaryOp(Lit(n1), _, Lit(n2)))
    }

  def all: ExprLexer.Parser[Option[Expr]] = arithmetic | power | justNum
}

object Wordy {
  def answer(sentence: String): Option[Int] =
    ExprLexer.parse(ExprLexer.all, sentence).get.flatMap(Expr.eval)
}
