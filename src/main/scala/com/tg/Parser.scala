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
  final case class Eq(left: Exp, right: Exp) extends Pred
  final case class Neq(left: Exp, right: Exp) extends Pred

  sealed trait Exp
  final case class ColName(n: String) extends Exp

  def command: Parser[Command] = (createTable | dropTable | insert | select | delete) <~ ";"

  def createTable: Parser[CreateTable] = "create" ~> "table" ~> identifier ~ ("(" ~> repsep(defCol, ",") <~ ")") ^^ {i => CreateTable(i._1, i._2)}
  def dropTable: Parser[DropTable] = "drop" ~> "table" ~> identifier ^^ {i => DropTable(i)}
  def insert: Parser[Insert] = "insert" ~> "into" ~> identifier ~ ("values" ~> ("(" ~> repsep(lit, ",") <~ ")")) ^^ {i => Insert(i._1, i._2)}
  def select: Parser[Select] = "select" ~> "*" ~> "from" ~> identifier ~ ("where" ~> pred) ^^ { case n~p => Select(n, p)}
  def delete: Parser[Delete] = "delete" ~> "from" ~> identifier ~ ("where" ~> pred) ^^ { case n~p => Delete(n, p)}

  def identifier: Parser[String] = """[_a-zA-Z][_a-zA-Z0-9]*""".r ^^ {_.toString}
  def colType: Parser[ColType] = int | double | string
  def int: Parser[ColType] = "int" ^^^ ColInt
  def double: Parser[ColType] = "double" ^^^ ColDouble
  def string: Parser[ColType] = "string" ^^^ ColString

  def defCol: Parser[(String, ColType)] = identifier ~ colType ^^ {i => (i._1, i._2)}

  def lit: Parser[Lit] = litDbl | litInt | litStr
  def litInt: Parser[LitInt] = """\d+""".r ^^ {i => LitInt(i.toInt)}
  def litDbl: Parser[LitDbl] = """\d*\.\d+""".r ^^ { i => LitDbl(i.toDouble) }
  def litStr: Parser[LitStr] = "'" ~> """[^']*""".r <~ "'" ^^ LitStr

  def exp: Parser[Exp] = lit | (identifier ^^ ColName)

  def pred: Parser[Pred] = eq | neq
  def eq: Parser[Eq] = exp ~ ("=" ~> exp) ^^ { case l~r => Eq(l, r) }
  def neq: Parser[Neq] = exp ~ ("<>" ~> exp) ^^ { case l~r => Neq(l, r) }


  /**
    * Executor
    */
  def run(command: Command): Unit = command match {
    case CreateTable(tblName, cols) => doCreateTable(tblName, cols)
    case DropTable(tblName) => doDropTable(tblName)
    case Insert(tblName, values) => doInsert(tblName, values)
    case Select(tblName, pred) => doSelect(tblName, pred)
    case Delete(tblName, pred) => doDelete(tblName, pred)
  }

  var tables: Map[String, (List[(String, ColType)], List[List[Lit]])] = Map()

  def doCreateTable(tblName: String, cols: List[(String, ColType)]): Unit = {
    tables = tables.updated(tblName, (cols, List()))
    println("create table successful!")
  }

  def doDropTable(tblName: String): Unit = {
    tables = tables - tblName
    println("drop table successful!")
  }

  def doInsert(tblName: String, values: List[Lit]): Unit = {
    tables.get(tblName).map(i => (i._1, values :: i._2)).foreach(
      newVs => tables = tables.updated(tblName, newVs)
    )
    println("insert successful!")
  }

  def doSelect(tblName: String, pred: Pred): Unit = {
    tables.get(tblName).foreach(i =>
      pred match {
        case Eq(ColName(n), r) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          println(i._2.filter(rcd => rcd(idx) == r).map(i => i.map(_.show()).mkString(", ")).mkString("\n"))
        case Eq(l, ColName(n)) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          println(i._2.filter(rcd => rcd(idx) == l).map(i => i.map(_.show()).mkString(", ")).mkString("\n"))
        case Neq(ColName(n), r) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          println(i._2.filter(rcd => rcd(idx) != r).map(i => i.map(_.show()).mkString(", ")).mkString("\n"))
        case Neq(l, ColName(n)) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          println(i._2.filter(rcd => rcd(idx) != l).map(i => i.map(_.show()).mkString(", ")).mkString("\n"))
      }
    )
  }

  def doDelete(tblName: String, pred: Pred): Unit = {
    tables.get(tblName).foreach(i =>
      pred match {
        case Eq(ColName(n), r) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          tables = tables.updated(tblName, (i._1, i._2.filterNot(rcd => rcd(idx) == r)))
        case Eq(l, ColName(n)) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          tables = tables.updated(tblName, (i._1, i._2.filterNot(rcd => rcd(idx) == l)))
        case Neq(ColName(n), r) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          tables = tables.updated(tblName, (i._1, i._2.filterNot(rcd => rcd(idx) != r)))
        case Neq(l, ColName(n)) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          tables = tables.updated(tblName, (i._1, i._2.filterNot(rcd => rcd(idx) != l)))
      }
    )
    println("delete successful!")
  }

  def main(args: Array[String]): Unit = {
//    val cs = List(
//      "create table t1 (id int, name string, score double);",
//      "insert into t1 values (0, 'tangent', 11.1);",
//      "select * from t1 where name = 'tangent';",
//      "insert into t1 values (1, 'gaufoo', 11.1);",
//      "insert into t1 values (2, 'doris', 11.1);",
//      "select * from t1 where score = 11.1;",
//      "delete from t1 where id = 1;",
//      "select * from t1 where id = 1;",
//      "drop table t1;"
//    )
//    cs.map(c => parse(command, c).get).foreach(run)

    while (true) {
      print(">> ")
      val str = scala.io.StdIn.readLine()
      run(parse(command, str).get)
      println()
    }
  }
}
