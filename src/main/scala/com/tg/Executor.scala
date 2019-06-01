package com.tg

object Executor {
  import com.tg.Parser._

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
    tables.get(tblName).foreach(i => {
      def printAll(): Unit = println(i._2.map(_.map(_.show()).mkString(", ")).mkString("\n"))

      pred match {
        case Eq(ColName(n), r) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          println(i._2.filter(rcd => rcd(idx) == r).map(i => i.map(_.show()).mkString(", ")).mkString("\n"))
        case Eq(l, ColName(n)) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          println(i._2.filter(rcd => rcd(idx) == l).map(i => i.map(_.show()).mkString(", ")).mkString("\n"))
        case Eq(l, r) => if (l == r) printAll()
        case Neq(ColName(n), r) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          println(i._2.filter(rcd => rcd(idx) != r).map(i => i.map(_.show()).mkString(", ")).mkString("\n"))
        case Neq(l, ColName(n)) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          println(i._2.filter(rcd => rcd(idx) != l).map(i => i.map(_.show()).mkString(", ")).mkString("\n"))
        case Neq(l, r) => if (l != r) printAll()
        case AlwaysTrue() => printAll()
      }
    }
    )
  }

  def doDelete(tblName: String, pred: Pred): Unit = {
    tables.get(tblName).foreach(i => {
      def deleteAll(): Unit = tables = tables.updated(tblName, (i._1, List()))
      pred match {
        case Eq(ColName(n), r) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          tables = tables.updated(tblName, (i._1, i._2.filterNot(rcd => rcd(idx) == r)))
        case Eq(l, ColName(n)) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          tables = tables.updated(tblName, (i._1, i._2.filterNot(rcd => rcd(idx) == l)))
        case Eq(l, r) => if (l == r) deleteAll()
        case Neq(ColName(n), r) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          tables = tables.updated(tblName, (i._1, i._2.filterNot(rcd => rcd(idx) != r)))
        case Neq(l, ColName(n)) =>
          val idx = i._1.indexWhere(w => w._1 == n)
          tables = tables.updated(tblName, (i._1, i._2.filterNot(rcd => rcd(idx) != l)))
        case Neq(l, r) => if (l != r) deleteAll()
        case AlwaysTrue() => deleteAll()
      }
    }
    )
    println("delete successful!")
  }

  def main(args: Array[String]): Unit = {
    val cs = List(
      "create table t1 (id int, name string, score double);",
      "insert into t1 values (0, 'tangent', 11.1);",
      "select * from t1 where name = 'tangent';",
      "insert into t1 values (1, 'gaufoo', 11.1);",
      "insert into t1 values (2, 'doris', 11.1);",
      "select * from t1 where name <> 'tangent';",
      "select * from t1;",
      "delete from t1 where id = 1;",
      "select * from t1;",
      "delete from t1;",
      "select * from t1;",
      "drop table t1;"
    )
    cs.map(c => (c, parse(command, c).get)).foreach(r => {
      println(s">> ${r._1}")
      run(r._2)
      println()
    })

    //    while (true) {
    //      print(">> ")
    //      val str = scala.io.StdIn.readLine()
    //      run(parse(command, str).get)
    //      println()
    //    }
  }
}
