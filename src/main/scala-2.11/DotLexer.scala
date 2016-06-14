import scala.io.Source._

object ParseExpr extends Expressions {
  def main(args: Array[String]) {
    val programText = fromFile(args(0)).mkString
    val res = parseAll(Expression, programText)
    println("input : "+ programText)
    res match {
      case Success(expr, _) => println(expr)
      case Failure(msg, _) => println("Parse error " + msg)
      case Error(msg, _) => println("Fatal Error " + msg)
    }
  }
}