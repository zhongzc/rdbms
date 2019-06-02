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
    case Select(newCols, tblNames, pred) => doSelect(wrapColName(newCols), checkTbl(tblNames), wrapColName(pred, tblNames))
    case Delete(tblName, pred) => doDelete(tblName, wrapColName(pred, List(tblName)))
  }

  private def wrapColName(exp: Exp, tbls: List[Table]): Exp = exp match {
    case ColName(l, None) =>
      val tblsWithThatName = tbls.filter(_.colTypes.map(_._1).contains(l))
      if (tblsWithThatName.length == 1) ColName(l, Option(tblsWithThatName.head.name))
      else sys.error(s"Can not match column name: $l")
    case c@ColName(l, Some(r)) =>
      if (tbls.find(_.name == r).exists(_.colTypes.map(_._1).contains(l)))
        c
      else sys.error(s"Can not match column name: $l in $r")
    case o => o
  }

  private def wrapColName(newCols: NewCols): NewCols = {
    if (newCols == All) All
    else NewCols(
      newCols.cols.map(wrapColName(_, tables.values.toList).asInstanceOf[ColName])
    )
  }

  private def wrapColName(pred: Cond, tblNames: List[String]): Cond = {
    // FIXME: May Cause A Runtime Error
    val tbls: List[Table] = tblNames.map(n => tables(n))

    pred.wrapColName(wrapColName(_, tbls))
  }

  private def checkTbl(tblNames: List[String]): List[String] = {
    val names = tables.keys.toSet
    if (tblNames.forall(t => names contains t)) tblNames
    else sys.error(s"Can not find table name: ${tblNames.mkString(", ")}")
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

  def doSelect(newCols: NewCols, tblNames: List[String], cond: Cond): Unit = {
    val relTbls: List[Table] = tblNames.map(tables(_))
    val recordsWithTbl: List[List[(Table, Vector[Lit])]] = relTbls.map(t => t.allRecords.map((t, _)).toList)
    val cartesianProd: List[Vector[(Table, Vector[Lit])]] = recordsWithTbl.tail.foldLeft(recordsWithTbl.head.map(Vector(_)))((acc, elem) =>
      for { x <- acc; y <- elem } yield x :+ y
    )
    cartesianProd.foreach( vec => {
      def fun(exp: Exp): Lit = exp match {
        case ColName(n, Some(t)) => vec.find(_._1.name == t).map{ case (tb, v) => tb.getValue(n, v) }.get
        case ColName(n, None) => sys.error(s"Can not match column name: $n") // Never Get Here
        case o: Lit => o
      }
      if (cond.pred(fun)(Lit.ordering)) {
        val cols =
          if (newCols == All) vec.flatMap(i => i._1.colTypes.map(j => ColName(j._1, Option(i._1.name))))
          else newCols.cols
        println(cols.map{ case ColName(cn, Some(tn)) =>
          val t = vec.find(_._1.name == tn)
          t.map(v => v._1.getValue(cn, v._2)).get.show()
        }.mkString(", "))
      }
    })
  }

  def doDelete(tblName: String, cond: Cond): Unit = {
    tables.get(tblName).foreach(t => {
        def pred(rc: Vector[Lit]): Boolean = {
          def fun(exp: Exp): Lit = exp match {
            case ColName(n, _) => t.getValue(n, rc)
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
//      "CREATE TABLE t1 (id INT, name STRING, score DOUBLE);",
//      "INSERT INTO t1 VALUES (0, 'tangent', 11.1);",
//      "SELECT * FROM t1 WHERE name = 'tangent';",
//      "INSERT INTO t1 VALUES (1, 'gaufoo', 12.1);",
//      "INSERT INTO t1 VALUES (2, 'doris', 13.1);",
//      "SELECT * FROM t1 WHERE name <> 'tangent';",
//      "SELECT * FROM t1 WHERE score >= 12;",
//      "SELECT id, score FROM t1 WHERE score > 13;",
//      "SELECT * FROM t1 WHERE score < 12 OR name = 'doris';",
//      "SELECT name FROM t1;",
//      "DELETE FROM t1 WHERE id = 1;",
//      "DELETE FROM t1 WHERE id = 1;",
//      "SELECT * FROM t1;",
//      "DELETE FROM t1 WHERE id >= 0;",
//      "SELECT * FROM t1;",
//      "DROP TABLE t1;",

      "CREATE TABLE students (id INT, age INT, name STRING);",
      "CREATE TABLE courses (id INT, name STRING);",
      "CREATE TABLE sc (sid INT, cid INT);",
      "INSERT INTO courses VALUES (0, '语文');",
      "INSERT INTO courses VALUES (1, '数学');",
      "INSERT INTO students VALUES (0, 18, '小明');",
      "INSERT INTO students VALUES (1, 19, '小红');",
      "INSERT INTO students VALUES (2, 19, '小乐');",
      "INSERT INTO sc VALUES (0, 0);",
      "INSERT INTO sc VALUES (0, 1);",
      "INSERT INTO sc VALUES (1, 0);",
      "INSERT INTO sc VALUES (2, 1);",
      "SELECT * FROM students;",
      "SELECT * FROM courses;",
      "SELECT * FROM sc;",
      """|SELECT students.name, courses.name
         |FROM students, courses, sc
         |WHERE students.id  = sc.sid
         |  AND courses.id   = sc.cid
         |  AND courses.name = '语文';""".stripMargin,
      """|SELECT students.name, age, courses.name
         |FROM students, courses, sc
         |WHERE students.id   = sc.sid
         |  AND courses.id    = sc.cid
         |  AND students.name = '小明';""".stripMargin,
      "DELETE FROM sc WHERE sid = 0;",
      """|SELECT students.name, age, courses.name
         |FROM students, courses, sc
         |WHERE students.id = sc.sid
         |  AND courses.id  = sc.cid;""".stripMargin,
    )
    cs.view.map(c => (c, parse(command, c))).foreach(r => {
      println(r._1.split("\n").map(s => s"~> $s").mkString("\n"))
      run(r._2.get)
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
