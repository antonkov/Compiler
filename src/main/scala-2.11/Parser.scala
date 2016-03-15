/**
  * Created by antonkov on 3/15/2016.
  */
import scala.util.parsing.combinator._

class LoopParser extends RegexParsers {
  override type Elem = Char
  def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r
  def integer     = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def loop =
    "for"~identifier~"in"~integer~"to"~integer~statement ^^
      { case f~variable~i~lBound~t~uBound~statement => ForLoop(variable, lBound, uBound,statement) }
  def statements = statement*
  def block = "{"~>statements<~"}"  ^^ { l => Block(l) }
  def statement : Parser[Statement] = loop | block
}

abstract trait Statement
case class Block(statements : List[Statement]) extends Statement
case class ForLoop(variable: String, lowerBound:Int, upperBound: Int, statement:Statement) extends Statement

object TestLoopParser extends LoopParser {
  parseAll(loop, "for x in 1 to 42 { for y in 0 to 1 {} }") match {
    case Success(lup,_) => println(lup)
    case x => println(x)
  }
}

object Parser {
  def main(args: Array[String]): Unit = {
    TestLoopParser.parseAll
  }
}