import sun.tools.tree.{ConditionalExpression, UnaryExpression}

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by antonkov on 6/14/16.
  */
trait Expressions extends JavaTokenParsers {

  class ExpressionNode {}

  class AdditiveExpressionNode(left: MultiplicativeExpressionNode, operator: Option[Operator] = None, right: Option[AdditiveExpressionNode] = None) extends ExpressionNode {
    override def toString = operator match {
      case Some(op) => s"($left ${op.symbol} ${right.get})"
      case None => s"$left"
    }
  }

  class MultiplicativeExpressionNode(left: UnaryExpressionNode, operator: Option[Operator] = None, right: Option[MultiplicativeExpressionNode] = None) extends ExpressionNode {
    override def toString = operator match {
      case Some(op) => s"($left ${op.symbol} ${right.get})"
      case None => s"$left"
    }
  }

  class UnaryExpressionNode extends ExpressionNode {}

  class PostfixExpressionNode extends UnaryExpressionNode {}

  class LeftSideExpressionNode extends PostfixExpressionNode {}

  class CallExpressionNode extends LeftSideExpressionNode {}

  class PrimaryExpressionNode extends CallExpressionNode {}

  sealed class SimpleExpressionNode extends PrimaryExpressionNode {}

  case class NumberNode(number: Double) extends SimpleExpressionNode {
    override def toString = number.toString
  }

  case class ParenthesizedExpressionNode(child: ExpressionNode) extends SimpleExpressionNode {
    override def toString = s"($child)"
  }


  // Base Parsers

  def Number: Parser[NumberNode] = floatingPointNumber ^^ { case number => new NumberNode(number.toString.toDouble) }

  // Expressions

  def Expression: Parser[Any] = AssignmentExpression

  // Primary Expressions

  def PrimaryExpression: Parser[PrimaryExpressionNode] = SimpleExpression

  def SimpleExpression = Number | ParenthesizedExpression

  def ParenthesizedExpression = "(" ~> Expression <~ ")" ^^ {
    case (child: ExpressionNode) => new ParenthesizedExpressionNode(child)
  }

  // Conditional Operators

  def ConditionalExpression = ternaryOperator | LogicalOrExpression

  def ternaryOperator = LogicalOrExpression ~ "?" ~ AssignmentExpression ~ ":" ~ AssignmentExpression

  // Assignment Operators

  def AssignmentExpression: Parser[Any] = assignment | ConditionalExpression

  def assignment: Parser[Any] = LeftSideExpression ~ "=" ~ AssignmentExpression

  // Left-Side expressions

  def LeftSideExpression = CallExpression

  def CallExpression = PrimaryExpression

  // Unary Operators

  def UnaryExpression = PostfixExpression

  def PostfixExpression = LeftSideExpression

  // Equality Operators

  def EqualityExpression: Parser[Any] = equalValue | notEqualValue | RelationalExpression

  def equalValue = RelationalExpression ~ "==" ~ EqualityExpression

  def notEqualValue = RelationalExpression ~ "!=" ~ EqualityExpression

  // Relational Operators

  def RelationalExpression: Parser[Any] = lower | greater | lowerEqual | greaterEqual | ShiftExpression

  def lower = ShiftExpression ~ "<" ~ RelationalExpression

  def greater = ShiftExpression ~ ">" ~ RelationalExpression

  def lowerEqual = ShiftExpression ~ "<=" ~ RelationalExpression

  def greaterEqual = ShiftExpression ~ ">=" ~ RelationalExpression

  // Binary Shift Operators

  def ShiftExpression: Parser[Any] = shiftLeft | shiftRight | arithmeticShiftRight | AdditiveExpression

  def shiftLeft = AdditiveExpression ~ "<<" ~ ShiftExpression

  def shiftRight = AdditiveExpression ~ ">>" ~ ShiftExpression

  def arithmeticShiftRight = AdditiveExpression ~ ">>>" ~ ShiftExpression


  // Additive Operators

  def AdditiveExpression: Parser[AdditiveExpressionNode] = addition | subtraction | singleMultiplicativeExpression

  def singleMultiplicativeExpression: Parser[AdditiveExpressionNode] = MultiplicativeExpression ^^ {
    case child: MultiplicativeExpressionNode => new AdditiveExpressionNode(child, None, None)
  }

  def addition: Parser[AdditiveExpressionNode] = MultiplicativeExpression ~ "+" ~ AdditiveExpression ^^ {
    case left ~ "+" ~ right => new AdditiveExpressionNode(left, Some(Operator.PLUS), Some(right))
  }

  def subtraction: Parser[AdditiveExpressionNode] = MultiplicativeExpression ~ "-" ~ AdditiveExpression ^^ {
    case left ~ "-" ~ right => new AdditiveExpressionNode(left, Some(Operator.MINUS), Some(right))
  }

  // Multiplicative Operators

  def MultiplicativeExpression: Parser[MultiplicativeExpressionNode] = multiplication | division | modulo | unaryExpression

  def unaryExpression: Parser[MultiplicativeExpressionNode] = UnaryExpression ^^ {
    case child => new MultiplicativeExpressionNode(child, None, None)
  }

  def multiplication: Parser[MultiplicativeExpressionNode] = UnaryExpression ~ "*" ~ MultiplicativeExpression ^^ {
    case left ~ "*" ~ right => new MultiplicativeExpressionNode(left, Some(Operator.MULTIPLY), Some(right))
  }

  def division: Parser[MultiplicativeExpressionNode] = UnaryExpression ~ "/" ~ MultiplicativeExpression ^^ {
    case left ~ "/" ~ right => new MultiplicativeExpressionNode(left, Some(Operator.DIVIDE), Some(right))
  }

  def modulo: Parser[MultiplicativeExpressionNode] = UnaryExpression ~ "%" ~ MultiplicativeExpression ^^ {
    case left ~ "%" ~ right => new MultiplicativeExpressionNode(left, Some(Operator.MODULO), Some(right))
  }

  // Binary Logical Operators

  def LogicalAndExpression: Parser[Any] = logicalAnd | BitwiseOrExpression

  def logicalAnd = BitwiseOrExpression ~ "&&" ~ LogicalAndExpression

  def LogicalOrExpression: Parser[Any] = logicalOr | LogicalAndExpression

  def logicalOr = LogicalAndExpression ~ "||" ~ LogicalOrExpression

  // Binary Bitwise Operators

  def BitwiseAndExpression: Parser[Any] = bitwiseAnd | EqualityExpression

  def bitwiseAnd = EqualityExpression ~ "&" ~ BitwiseAndExpression

  def BitwiseXorExpression: Parser[Any] = bitwiseXor | BitwiseAndExpression

  def bitwiseXor = BitwiseAndExpression ~ "^" ~ BitwiseXorExpression

  def BitwiseOrExpression: Parser[Any] = bitwiseOr | BitwiseXorExpression

  def bitwiseOr = BitwiseXorExpression ~ "|" ~ BitwiseOrExpression
}
