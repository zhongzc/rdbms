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
  final case class Select(tblName: String, cond: Cond) extends Command
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
  }
  final case object AlwaysTrue extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = true
  }
  final case class Eq(left: Exp, right: Exp) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = env(left) == env(right)
  }
  final case class LT(left: Exp, right: Exp) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = env(left) < env(right)
  }
  final case class GT(left: Exp, right: Exp) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = env(left) > env(right)
  }
  final case class Not(e: Cond) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = !e.pred(env)
  }
  final case class And(l: Cond, r: Cond) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = l.pred(env) && r.pred(env)
  }
  final case class Or(l: Cond, r: Cond) extends Cond {
    override def pred[T: Ordering](env: Exp => T): Boolean = l.pred(env) || r.pred(env)
  }

  sealed trait Exp
  final case class ColName(n: String) extends Exp

  def command: Parser[Command] = (createTable | dropTable | insert | select | delete) <~ ";"

  def createTable: Parser[CreateTable] = "create" ~> "table" ~> identifier ~ ("(" ~> repsep(defCol, ",") <~ ")") ^^ {i => CreateTable(i._1, i._2)}
  def dropTable: Parser[DropTable] = "drop" ~> "table" ~> identifier ^^ DropTable
  def insert: Parser[Insert] = "insert" ~> "into" ~> identifier ~ ("values" ~> ("(" ~> repsep(lit, ",") <~ ")")) ^^ {i => Insert(i._1, i._2)}
  def select: Parser[Select] = "select" ~> "*" ~> "from" ~> identifier ~ cond ^^ { case n~p => Select(n, p)}
  def delete: Parser[Delete] = "delete" ~> "from" ~> identifier ~ cond ^^ { case n~p => Delete(n, p)}

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

  def cond: Parser[Cond] = ("where" ~> orChain) | alwaysTrue
  def alwaysTrue: Parser[AlwaysTrue.type] = success(AlwaysTrue)
  def orChain: Parser[Cond] = chainl1(andChain, "or" ^^^ ((l: Cond, r: Cond) => Or(l, r)))
  def andChain: Parser[Cond] = chainl1(simpleCond, "and" ^^^ ((l: Cond, r: Cond) => And(l, r)))
  def simpleCond: Parser[Cond] = eq | ne | lt | ge | gt | le | "(" ~> orChain <~ ")"
  def eq: Parser[Eq] = exp ~ ("=" ~> exp) ^^ { case l~r => Eq(l, r) }
  def ne: Parser[Cond] = exp ~ (("<>" | "!=") ~> exp) ^^ { case l~r => Not(Eq(l, r)) }
  def lt: Parser[LT] = exp ~ ("<" ~> exp) ^^ { case l~r => LT(l, r) }
  def ge: Parser[Cond] = exp ~ (">=" ~> exp) ^^ { case l~r => Not(LT(l, r)) }
  def gt: Parser[GT] = exp ~ (">" ~> exp) ^^ { case l~r => GT(l, r) }
  def le: Parser[Cond] = exp ~ ("<=" ~> exp) ^^ { case l~r => Not(GT(l, r)) }

  def main(args: Array[String]): Unit = {
    println(parseAll(command, "select * from t2 where i > 2 and g <= 3 or 33 != 23;"))
  }
}
