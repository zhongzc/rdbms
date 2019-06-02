package com.tg

import scala.math.Ordering.Implicits._
import scala.util.parsing.combinator._

object Parser extends RegexParsers {

  /**
    * Parser
    */
  sealed trait Command
  final case class CreateTable(tblName: String, cols: List[(String, ColType)]) extends Command
  final case class DropTable(tblName: String) extends Command
  final case class Insert(tblName: String, values: List[Lit]) extends Command
  final case class Select(newCols: NewCols, tblNames: List[String],  cond: Cond) extends Command
  final case class Delete(tblName: String, cond: Cond) extends Command

  sealed trait ColType {
//    def fromString(string: String): Option[Lit]
  }
  final case object ColInt extends ColType {
//    override def fromString(string: String): Option[Lit] =
//      try { Option(LitInt(string.toInt)) } catch { case _ => None }
  }
  final case object ColDouble extends ColType {
//    override def fromString(string: String): Option[Lit] =
//      try { Option(LitDbl(string.toDouble)) } catch { case _ => None }
  }
  final case object ColString extends ColType {
//    override def fromString(string: String): Option[Lit] =
//      Option(LitStr(string))
  }

  class NewCols(val cols: List[ColName])
  object NewCols {
    def apply(cols: List[ColName]): NewCols = new NewCols(cols)
  }
  case object All extends NewCols(List())

  sealed trait Lit extends Exp {
    def show(): String
    def is(colType: ColType): Boolean
  }
  final case class LitInt(v: Int) extends Lit {
    override def show(): String = v.toString
    override def is(colType: ColType): Boolean = colType == ColInt
  }
  final case class LitDbl(v: Double) extends Lit {
    override def show(): String = v.toString
    override def is(colType: ColType): Boolean = colType == ColDouble
  }
  final case class LitStr(v: String) extends Lit {
    override def show(): String = s"'$v'"
    override def is(colType: ColType): Boolean = colType == ColString
  }
  object Lit {
    implicit def ordering: Ordering[Lit] = (x: Lit, y: Lit) => (x, y) match {
      case (LitStr(l), LitStr(r)) => implicitly[Ordering[String]].compare(l, r)
      case (LitInt(l), LitInt(r)) => implicitly[Ordering[Int]].compare(l, r)
      case (LitDbl(l), LitDbl(r)) => implicitly[Ordering[Double]].compare(l, r)
      case (LitInt(l), LitDbl(r)) => if (l < r) -1 else if (l == r) 0 else 1
      case (LitDbl(l), LitInt(r)) => if (l < r) -1 else if (l == r) 0 else 1
      case (l, r) => sys.error(s"Can not compare $l and $r")
    }
  }

  sealed trait Cond {
    def pred[T: Ordering](env: Exp => T): Boolean
    def wrapColName(fn: Exp => Exp): Cond
  }
  final case object AlwaysTrue extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = true
    override def wrapColName(fn: Exp => Exp): Cond = this
  }
  final case class Eq(left: Exp, right: Exp) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = env(left) == env(right)
    override def wrapColName(fn: Exp => Exp): Cond = Eq(fn(left), fn(right))
  }
  final case class LT(left: Exp, right: Exp) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = env(left) < env(right)
    override def wrapColName(fn: Exp => Exp): Cond = LT(fn(left), fn(right))
  }
  final case class GT(left: Exp, right: Exp) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = env(left) > env(right)
    override def wrapColName(fn: Exp => Exp): Cond = GT(fn(left), fn(right))
  }
  final case class Not(e: Cond) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = !e.pred(env)
    override def wrapColName(fn: Exp => Exp): Cond = Not(e.wrapColName(fn))
  }
  final case class And(l: Cond, r: Cond) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = l.pred(env) && r.pred(env)
    override def wrapColName(fn: Exp => Exp): Cond = And(l.wrapColName(fn), r.wrapColName(fn))
  }
  final case class Or(l: Cond, r: Cond) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = l.pred(env) || r.pred(env)
    override def wrapColName(fn: Exp => Exp): Cond = Or(l.wrapColName(fn), r.wrapColName(fn))
  }

  sealed trait Exp
  final case class ColName(n: String, tblAlign: Option[String]) extends Exp

  def command: Parser[Command] = (createTable | dropTable | insert | select | delete) <~ ";"

  def createTable: Parser[CreateTable] = "CREATE" ~> "TABLE" ~> identifier ~ ("(" ~> repsep(defCol, ",") <~ ")") ^^ {i => CreateTable(i._1, i._2)}
  def dropTable: Parser[DropTable] = "DROP" ~> "TABLE" ~> identifier ^^ DropTable
  def insert: Parser[Insert] = "INSERT" ~> "INTO" ~> identifier ~ ("VALUES" ~> ("(" ~> repsep(lit, ",") <~ ")")) ^^ {i => Insert(i._1, i._2)}
  def select: Parser[Select] = "SELECT" ~> newCols ~ ("FROM" ~> rep1sep(identifier, ",")) ~ cond ^^ { case c~n~p => Select(c, n, p)}
  def delete: Parser[Delete] = "DELETE" ~> "FROM" ~> identifier ~ cond ^^ { case n~p => Delete(n, p)}

  def identifier: Parser[String] = """[_a-zA-Z][_a-zA-Z0-9]*""".r ^^ {_.toString}
  def colType: Parser[ColType] = "INT"    ^^^ ColInt    |
                                 "DOUBLE" ^^^ ColDouble |
                                 "STRING" ^^^ ColString

  def defCol: Parser[(String, ColType)] = identifier ~ colType ^^ {i => (i._1, i._2)}

  def newCols: Parser[NewCols] = "*" ^^^ All | repsep(colName, ",") ^^ (i => NewCols(i))

  def lit: Parser[Lit] = litDbl | litInt | litStr
  def litInt: Parser[LitInt] = """\d+""".r ^^ {i => LitInt(i.toInt)}
  def litDbl: Parser[LitDbl] = """\d*\.\d+""".r ^^ { i => LitDbl(i.toDouble) }
  def litStr: Parser[LitStr] = "'" ~> """[^']*""".r <~ "'" ^^ LitStr

  def exp: Parser[Exp] = lit | colName
  def colName: Parser[ColName] = identifier ~ opt("." ~> identifier) ^^ {
    case l~Some(r) =>
      ColName(r, Option(l))
    case l~None =>
      ColName(l, None)
  }

  def cond: Parser[Cond] = ("WHERE" ~> orChain) | alwaysTrue
  def alwaysTrue: Parser[AlwaysTrue.type] = success(AlwaysTrue)
  def orChain: Parser[Cond] = chainl1(andChain, "OR" ^^^ ((l: Cond, r: Cond) => Or(l, r)))
  def andChain: Parser[Cond] = chainl1(simpleCond, "AND" ^^^ ((l: Cond, r: Cond) => And(l, r)))
  def simpleCond: Parser[Cond] = eq | ne | lt | ge | gt | le | "(" ~> orChain <~ ")"
  def eq: Parser[Eq] = exp ~ ("=" ~> exp) ^^ { case l~r => Eq(l, r) }
  def ne: Parser[Cond] = exp ~ (("<>" | "!=") ~> exp) ^^ { case l~r => Not(Eq(l, r)) }
  def lt: Parser[LT] = exp ~ ("<" ~> exp) ^^ { case l~r => LT(l, r) }
  def ge: Parser[Cond] = exp ~ (">=" ~> exp) ^^ { case l~r => Not(LT(l, r)) }
  def gt: Parser[GT] = exp ~ (">" ~> exp) ^^ { case l~r => GT(l, r) }
  def le: Parser[Cond] = exp ~ ("<=" ~> exp) ^^ { case l~r => Not(GT(l, r)) }

  def main(args: Array[String]): Unit = {
    println(parseAll(command, "SELECT * FROM t2 WHERE i > 2 AND g <= 3 OR 33 != 23;"))
  }
}
