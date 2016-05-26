import scala.io.Source._

object ParseExpr extends Arith {
  def main(args: Array[String]) {
    val programText = fromFile(args(0)).mkString
    val res = parseAll(expr, programText)
    println("input : "+ programText)
    res match {
      case Success(expr, _) => println(expr.calc())
      case Failure(msg, _) => println("Parse error" + msg)
    }
  }
}