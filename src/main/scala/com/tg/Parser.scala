package com.tg
import scala.util.parsing.combinator._

object Parser extends RegexParsers {

  /**
    * Parser
    */
  sealed trait Command
  final case class CreateTable(tblName: String, cols: List[(String, ColType)]) extends Command
  final case class DropTable(tblName: String) extends Command
  final case class Insert(tblName: String, values: List[Lit]) extends Command
  final case class Select(tblName: String, pred: Pred) extends Command
  final case class Delete(tblName: String, pred: Pred) extends Command

  sealed trait ColType
  final case object ColInt extends ColType
  final case object ColDouble extends ColType
  final case object ColString extends ColType

  sealed trait Lit extends Exp {
    def show(): String
  }
  final case class LitInt(v: Int) extends Lit {
    override def show(): String = v.toString
  }
  final case class LitDbl(v: Double) extends Lit {
    override def show(): String = v.toString
  }
  final case class LitStr(v: String) extends Lit {
    override def show(): String = v
  }

  sealed trait Pred
  final case class AlwaysTrue() extends Pred
  final case class Eq(left: Exp, right: Exp) extends Pred
  final case class Neq(left: Exp, right: Exp) extends Pred

  sealed trait Exp
  final case class ColName(n: String) extends Exp

  def command: Parser[Command] = (createTable | dropTable | insert | select | delete) <~ ";"

  def createTable: Parser[CreateTable] = "create" ~> "table" ~> identifier ~ ("(" ~> repsep(defCol, ",") <~ ")") ^^ {i => CreateTable(i._1, i._2)}
  def dropTable: Parser[DropTable] = "drop" ~> "table" ~> identifier ^^ {i => DropTable(i)}
  def insert: Parser[Insert] = "insert" ~> "into" ~> identifier ~ ("values" ~> ("(" ~> repsep(lit, ",") <~ ")")) ^^ {i => Insert(i._1, i._2)}
  def select: Parser[Select] = "select" ~> "*" ~> "from" ~> identifier ~ pred ^^ { case n~p => Select(n, p)}
  def delete: Parser[Delete] = "delete" ~> "from" ~> identifier ~ pred ^^ { case n~p => Delete(n, p)}

  def identifier: Parser[String] = """[_a-zA-Z][_a-zA-Z0-9]*""".r ^^ {_.toString}
  def colType: Parser[ColType] = "int"    ^^^ ColInt    |
                                 "double" ^^^ ColDouble |
                                 "string" ^^^ ColString

  def defCol: Parser[(String, ColType)] = identifier ~ colType ^^ {i => (i._1, i._2)}

  def lit: Parser[Lit] = litDbl | litInt | litStr
  def litInt: Parser[LitInt] = """\d+""".r ^^ {i => LitInt(i.toInt)}
  def litDbl: Parser[LitDbl] = """\d*\.\d+""".r ^^ { i => LitDbl(i.toDouble) }
  def litStr: Parser[LitStr] = "'" ~> """[^']*""".r <~ "'" ^^ LitStr

  def exp: Parser[Exp] = lit | (identifier ^^ ColName)

  def pred: Parser[Pred] = ("where" ~> (eq | neq)) | alwaysTrue
  def alwaysTrue: Parser[AlwaysTrue] = success(AlwaysTrue())
  def eq: Parser[Eq] = exp ~ ("=" ~> exp) ^^ { case l~r => Eq(l, r) }
  def neq: Parser[Neq] = exp ~ ("<>" ~> exp) ^^ { case l~r => Neq(l, r) }
}
