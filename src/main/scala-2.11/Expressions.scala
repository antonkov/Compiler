import sun.tools.tree.{ConditionalExpression, UnaryExpression}

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by antonkov on 6/14/16.
  */
trait Expressions extends JavaTokenParsers {

  class ExpressionNode {}

  class BinaryOperatorNode(left: ExpressionNode, operator: Operator, right: ExpressionNode) extends ExpressionNode {
    override def toString = s"($left ${operator.symbol} $right)"
  }

  class UnaryOperatorNode(child: ExpressionNode, operator: Operator) extends ExpressionNode {
    override def toString = s"${operator.symbol}$child"
  }

  class AssignmentExpressionNode(leftSideExpressions: List[ExpressionNode], expr: ExpressionNode) extends ExpressionNode {
    override def toString = {
      var s = ""
      for (node <- leftSideExpressions) {
        s += node + " = "
      }
      s += expr
      s
    }
  }

  class ConditionalExpressionNode(condition: ExpressionNode, actions: Option[(ExpressionNode, ExpressionNode)]) extends ExpressionNode {
    override def toString = actions match {
      case Some((trueAction, falseAction)) => s"(${condition} ? ${trueAction} : ${falseAction})"
      case None => s"$condition"
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

  case class IdentifierNode(name: String) extends SimpleExpressionNode {
    override def toString = name
  }

  case class ParenthesizedExpressionNode(child: ExpressionNode) extends SimpleExpressionNode {
    override def toString = s"($child)"
  }


  // Base Parsers

  def Number: Parser[NumberNode] = floatingPointNumber ^^ { case number => new NumberNode(number.toString.toDouble) }

  def Identifier: Parser[IdentifierNode] = ident ^^ { case identifier => new IdentifierNode(identifier.toString) }

  // Expressions

  def Expression: Parser[ExpressionNode] = AssignmentExpression

  // Primary Expressions

  def PrimaryExpression = SimpleExpression

  def SimpleExpression = Number | Identifier | ParenthesizedExpression

  def ParenthesizedExpression = "(" ~> Expression <~ ")" ^^ {
    case (child: ExpressionNode) => child
  }

  // Conditional Operators

  def ConditionalExpression: Parser[ExpressionNode] = LogicalOrExpression ~ opt("?" ~ AssignmentExpression ~ ":" ~ AssignmentExpression) ^^ {
    case expr ~ Some("?" ~ trueAction ~ ":" ~ falseAction) => new ConditionalExpressionNode(expr, Some((trueAction, falseAction)))
    case expr ~ None => new ConditionalExpressionNode(expr, None)
  }

  // Assignment Operators

  def AssignmentExpression: Parser[ExpressionNode] = rep(LeftSideExpression <~ "=") ~ ConditionalExpression ^^ {
    case leftSides ~ condition => new AssignmentExpressionNode(leftSides, condition)
  }

  // Left-Side expressions

  def LeftSideExpression = CallExpression

  def CallExpression = PrimaryExpression

  // Unary Operators

  def UnaryExpression = PostfixExpression

  def PostfixExpression = LeftSideExpression

  // Equality Operators

  def EqualityExpression: Parser[ExpressionNode] = RelationalExpression ~ rep(("==" | "!=") ~ RelationalExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "==" ~ y) => new BinaryOperatorNode(x, Operator.EQUAL_VALUE, y)
      case (x, "!=" ~ y) => new BinaryOperatorNode(x, Operator.NOT_EQUAL_VALUE, y)
    }
  }

  // Relational Operators

  def RelationalExpression: Parser[ExpressionNode] = ShiftExpression ~ rep(("<" | ">" | "<=" | ">=") ~ ShiftExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "<" ~ y) => new BinaryOperatorNode(x, Operator.LOWER, y)
      case (x, ">" ~ y) => new BinaryOperatorNode(x, Operator.GREATER, y)
      case (x, "<=" ~ y) => new BinaryOperatorNode(x, Operator.LOWER_EQUAL, y)
      case (x, ">=" ~ y) => new BinaryOperatorNode(x, Operator.GREATER_EQUAL, y)
    }
  }

  // Binary Shift Operators

  def ShiftExpression: Parser[ExpressionNode] = AdditiveExpression ~ rep(("<<" | ">>" | ">>>") ~ AdditiveExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "<<" ~ y) => new BinaryOperatorNode(x, Operator.SHIFT_LEFT, y)
      case (x, ">>" ~ y) => new BinaryOperatorNode(x, Operator.SHIFT_RIGHT, y)
      case (x, ">>>" ~ y) => new BinaryOperatorNode(x, Operator.ARITHMETIC_SHIFT_RIGHT, y)
    }
  }

  // Additive Operators

  def AdditiveExpression: Parser[ExpressionNode] = MultiplicativeExpression ~ rep(("+" | "-") ~ MultiplicativeExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "+" ~ y) => new BinaryOperatorNode(x, Operator.PLUS, y)
      case (x, "-" ~ y) => new BinaryOperatorNode(x, Operator.MINUS, y)
    }
  }

  // Multiplicative Operators

  def MultiplicativeExpression: Parser[ExpressionNode] = UnaryExpression ~ rep(("*" | "/" | "%") ~ UnaryExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "*" ~ y) => new BinaryOperatorNode(x, Operator.MULTIPLY, y)
      case (x, "/" ~ y) => new BinaryOperatorNode(x, Operator.DIVIDE, y)
      case (x, "%" ~ y) => new BinaryOperatorNode(x, Operator.MODULO, y)
    }
  }

  // Binary Logical Operators

  def LogicalAndExpression: Parser[ExpressionNode] = BitwiseOrExpression ~ rep("&&" ~ BitwiseOrExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "&&" ~ y) => new BinaryOperatorNode(x, Operator.PLUS, y)
    }
  }

  def LogicalOrExpression: Parser[ExpressionNode] = LogicalAndExpression ~ rep("||" ~ LogicalAndExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "||" ~ y) => new BinaryOperatorNode(x, Operator.LOGICAL_OR, y)
    }
  }

  // Binary Bitwise Operators

  def BitwiseAndExpression: Parser[ExpressionNode] = EqualityExpression ~ rep("&" ~ EqualityExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "&" ~ y) => new BinaryOperatorNode(x, Operator.BITWISE_AND, y)
    }
  }

  def BitwiseXorExpression: Parser[ExpressionNode] = BitwiseAndExpression ~ rep("^" ~ BitwiseAndExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "^" ~ y) => new BinaryOperatorNode(x, Operator.BITWISE_XOR, y)
    }
  }

  def BitwiseOrExpression: Parser[ExpressionNode] = BitwiseXorExpression ~ rep("|" ~ BitwiseXorExpression) ^^ {
    case x ~ xs => xs.foldLeft(x) {
      case (x, "|" ~ y) => new BinaryOperatorNode(x, Operator.BITWISE_OR, y)
    }
  }
}