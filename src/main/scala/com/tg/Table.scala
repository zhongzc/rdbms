package com.tg

import scala.collection.immutable.VectorIterator
import com.tg.Parser._

class Table(val name: String, val colTypes: List[(String, ColType)]) {
  private val colnameToLoc: Map[String, (Int, ColType)] =
    colTypes.zipWithIndex.map{ case ((cn, typ), loc) => cn -> (loc, typ) }.toMap

  private var records: Vector[Vector[Lit]] = Vector()

  def insert(wild: Vector[Lit]): Unit = {
    if (colTypes.length == wild.length) {
      val a = Vector.newBuilder[Lit]
      a.sizeHint(colTypes.length)
      val p = wild.zip(colTypes).forall{ case (w, (_, typ)) =>
        if (w is typ) { a += w; true } else false
      }
      if (p) records = records :+ a.result()
    }
  }

  def allRecords: VectorIterator[Vector[Lit]] = records.iterator

  def getValue(colName: String, record: Vector[Lit]): Lit = record(colnameToLoc(colName)._1)

  def deleteIf(pred: Vector[Lit] => Boolean): Unit = records = records.filterNot(pred)
}

object Table {
  def apply(name: String, colTypes: List[(String, ColType)]): Table =
    new Table(name, colTypes)
}
