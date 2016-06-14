/**
  * Created by antonkov on 6/14/16.
  */
class Operator private (val symbol: String) {
}

object Operator {
  val PLUS = new Operator("+")
  val MINUS = new Operator("-")
  val MULTIPLY = new Operator("*")
  val DIVIDE = new Operator("/")
  val MODULO = new Operator("%")
}
