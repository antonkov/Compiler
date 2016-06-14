import sun.tools.tree.UnaryExpression

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by antonkov on 6/14/16.
  */
trait Expressions extends JavaTokenParsers {

  class ExpressionNode

  class AdditiveExpressionNode(left: MultiplicativeExpressionNode, operator: Option[Operator] = None, right: Option[AdditiveExpressionNode] = None) extends ExpressionNode

  class MultiplicativeExpressionNode(left: UnaryExpressionNode, operator: Option[Operator] = None, right: Option[MultiplicativeExpressionNode] = None) extends ExpressionNode

  class UnaryExpressionNode extends ExpressionNode {}

  class PostfixExpressionNode extends UnaryExpressionNode {}

  class LeftSideExpressionNode extends PostfixExpressionNode {}

  class CallExpressionNode extends LeftSideExpressionNode {}

  class PrimaryExpressionNode extends CallExpressionNode {}

  sealed class SimpleExpressionNode extends PrimaryExpressionNode {}

  case class NumberNode(number: Double) extends SimpleExpressionNode

  case class ParenthesizedExpressionNode(child: ExpressionNode) extends SimpleExpressionNode


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

  def ConditionalExpression = LogicalOrExpression | ternaryOperator

  def ternaryOperator = LogicalOrExpression ~ "?" ~ AssignmentExpression ~ ":" ~ AssignmentExpression

  // Assignment Operators

  def AssignmentExpression: Parser[Any] = ConditionalExpression | assignment

  def assignment: Parser[Any] = LeftSideExpression ~ "=" ~ AssignmentExpression

  // Left-Side expressions

  def LeftSideExpression = CallExpression

  def CallExpression = PrimaryExpression

  // Unary Operators

  def UnaryExpression = PostfixExpression

  def PostfixExpression = LeftSideExpression

  // Equality Operators

  def EqualityExpression: Parser[Any] = RelationalExpression | equalValue | notEqualValue

  def equalValue = RelationalExpression ~ "==" ~ EqualityExpression

  def notEqualValue = RelationalExpression ~ "!=" ~ EqualityExpression

  // Relational Operators

  def RelationalExpression: Parser[Any] = lower | greater | lowerEqual | greaterEqual

  def lower = RelationalExpression ~ "<" ~ ShiftExpression

  def greater = RelationalExpression ~ ">" ~ ShiftExpression

  def lowerEqual = RelationalExpression ~ "<=" ~ ShiftExpression

  def greaterEqual = RelationalExpression ~ ">=" ~ ShiftExpression

  // Binary Shift Operators

  def ShiftExpression: Parser[Any] = AdditiveExpression | shiftLeft | shiftRight | arithmeticShiftRight

  def shiftLeft = AdditiveExpression ~ "<<" ~ ShiftExpression

  def shiftRight = AdditiveExpression ~ ">>" ~ ShiftExpression

  def arithmeticShiftRight = AdditiveExpression ~ ">>>" ~ ShiftExpression


  // Additive Operators

  def AdditiveExpression: Parser[AdditiveExpressionNode] = multiplicativeExpression | addition | subtraction

  def multiplicativeExpression: Parser[AdditiveExpressionNode] = MultiplicativeExpression ^^ {
    case child: MultiplicativeExpressionNode => new AdditiveExpressionNode(child, None, None)
  }

  def addition: Parser[AdditiveExpressionNode] = MultiplicativeExpression ~ "+" ~ AdditiveExpression ^^ {
    case left ~ "+" ~ right => new AdditiveExpressionNode(left, Some(Operator.PLUS), Some(right))
  }

  def subtraction: Parser[AdditiveExpressionNode] = MultiplicativeExpression ~ "-" ~ AdditiveExpression ^^ {
    case left ~ "-" ~ right => new AdditiveExpressionNode(left, Some(Operator.MINUS), Some(right))
  }

  // Multiplicative Operators

  def MultiplicativeExpression: Parser[MultiplicativeExpressionNode] = unaryExpression | multiplication | division | modulo

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

  def LogicalAndExpression: Parser[Any] = BitwiseOrExpression | logicalAnd

  def logicalAnd = BitwiseOrExpression ~ "&&" ~ LogicalAndExpression

  def LogicalOrExpression: Parser[Any] = LogicalAndExpression | logicalOr

  def logicalOr = LogicalAndExpression ~ "||" ~ LogicalOrExpression

  // Binary Bitwise Operators

  def BitwiseAndExpression: Parser[Any] = EqualityExpression | bitwiseAnd

  def bitwiseAnd = EqualityExpression ~ "&" ~ BitwiseAndExpression

  def BitwiseXorExpression: Parser[Any] = BitwiseAndExpression | bitwiseXor

  def bitwiseXor = BitwiseAndExpression ~ "^" ~ BitwiseXorExpression

  def BitwiseOrExpression: Parser[Any] = BitwiseXorExpression | bitwiseOr

  def bitwiseOr = BitwiseXorExpression ~ "|" ~ BitwiseOrExpression
}
