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
    case Select(tblName, newCols, pred) => doSelect(tblName, newCols, pred)
    case Delete(tblName, pred) => doDelete(tblName, pred)
  }

  var tables: Map[String, Table] = Map()

  def doCreateTable(tblName: String, cols: List[(String, ColType)]): Unit = {
    tables = tables.updated(tblName, Table(tblName, cols))
    println("create table successful!")
  }

  def doDropTable(tblName: String): Unit = {
    tables = tables - tblName
    println("drop table successful!")
  }

  def doInsert(tblName: String, values: List[Lit]): Unit = {
    tables.get(tblName).foreach(t => {
      t.insert(values.toVector)
      println("insert successful!")
    })
  }

  def doSelect(tblName: String, newCols: NewCols, cond: Cond): Unit = {
    tables.get(tblName).foreach(t => {
        t.allRecords.foreach(rc => {
          def fun(exp: Exp): Lit = exp match {
            case ColName(n) => t.getValue(n, rc)
            case o: Lit => o
          }
          if (cond.pred(fun)(Lit.ordering)) {
            val cols = if (newCols == All) t.colTypes.map(_._1) else newCols.cols
            println(cols.map(t.getValue(_, rc).show()).mkString(", "))
          }
        })
      }
    )
  }

  def doDelete(tblName: String, cond: Cond): Unit = {
    tables.get(tblName).foreach(t => {
        def pred(rc: Vector[Lit]): Boolean = {
          def fun(exp: Exp): Lit = exp match {
            case ColName(n) => t.getValue(n, rc)
            case o: Lit => o
          }
          cond.pred(fun)(Lit.ordering)
        }

        t.deleteIf(pred)
      }
    )
    println("delete successful!")
  }

  def main(args: Array[String]): Unit = {
    val cs = List(
      "create table t1 (id int, name string, score double);",
      "insert into t1 values (0, 'tangent', 11.1);",
      "select * from t1 where name = 'tangent';",
      "insert into t1 values (1, 'gaufoo', 12.1);",
      "insert into t1 values (2, 'doris', 13.1);",
      "select * from t1 where name <> 'tangent';",
      "select * from t1 where score >= 12;",
      "select id, score from t1 where score > 13;",
      "select * from t1 where score < 12;",
      "select name from t1;",
      "delete from t1 where id = 1;",
      "select * from t1;",
      "delete from t1 where id >= 0;",
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
