import scala.util.parsing.combinator._

sealed abstract class Expression {
  def calc(): Double
}

case class Number(value: Double) extends Expression {
  override def calc() = value
}

case class Add(x: Expression, y: Expression) extends Expression {
  override def calc() = x.calc() + y.calc()
}

case class Sub(x: Expression, y: Expression) extends Expression {
  override def calc() = x.calc() - y.calc()
}

case class Mul(x: Expression, y: Expression) extends Expression {
  override def calc() = x.calc() * y.calc()
}

case class Div(x: Expression, y: Expression) extends Expression {
  override def calc() = x.calc() / y.calc()
}

class Arith extends JavaTokenParsers {
  def expr: Parser[Expression] = term~rep("+"~term | "-"~term) ^^ {
    case op ~ list => list.foldLeft(op) {
      case (x, "+" ~ y) => Add(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }

  def term: Parser[Expression] = factor~rep("*"~factor | "/"~factor) ^^ {
    case op ~ list => list.foldLeft(op) {
      case (x, "*" ~ y) => Mul(x, y)
      case (x, "/" ~ y) => Div(x, y)
    }
  }

  def factor: Parser[Expression] = number | parens

  def parens: Parser[Expression] = "(" ~> expr <~ ")" ^^ {
    case expr => expr
  }

  def number: Parser[Expression] = floatingPointNumber ^^ {
    case number => Number(number.toString.toDouble)
  }
}