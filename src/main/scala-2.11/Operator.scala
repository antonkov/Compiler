/**
  * Created by antonkov on 6/14/16.
  */
class Operator private(val symbol: String) {
}

object Operator {
  val PLUS = new Operator("+")
  val MINUS = new Operator("-")
  val MULTIPLY = new Operator("*")
  val DIVIDE = new Operator("/")
  val MODULO = new Operator("%")
  val SHIFT_LEFT = new Operator("<<")
  val SHIFT_RIGHT = new Operator(">>")
  val ARITHMETIC_SHIFT_RIGHT = new Operator(">>>")
  val LOWER = new Operator("<")
  val GREATER = new Operator(">")
  val LOWER_EQUAL = new Operator("<=")
  val GREATER_EQUAL = new Operator(">=")
  val EQUAL_VALUE = new Operator("==")
  val NOT_EQUAL_VALUE = new Operator("!=")
  val LOGICAL_OR = new Operator("||")
  val LOGICAL_AND = new Operator("&&")
  val BITWISE_OR = new Operator("|")
  val BITWISE_AND = new Operator("&")
  val BITWISE_XOR = new Operator("^")
}
