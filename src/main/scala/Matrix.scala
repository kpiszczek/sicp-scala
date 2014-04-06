package kuba.sicp

import scala.language.implicitConversions

package object matrix {
  type Row = Vector[Double]
  type MatrixLike = Vector[Row]

  def dotProduct(v1: Row, v2: Row) = 
    v1.zip(v2).map(e => e._1 * e._2).reduceLeft(_ + _)

  case class Matrix(m: MatrixLike) {
    def *(that: Matrix) = 
      for (row <- m) yield {
        for (col <- that.transpose.m) 
        yield dotProduct(row, col)
      } 
    def transpose(): Matrix = 
      if (m.head.isEmpty) Matrix(Vector())
      else Matrix(m.map(_.head) +: Matrix(m.map(_.tail)).transpose.m)
  }

  object Matrix {
    implicit def rowToMatrix(row: Row): Matrix = Matrix(Vector(row))
  }
}