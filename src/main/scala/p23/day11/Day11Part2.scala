package p23.day11

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.io.Source

//noinspection DuplicatedCode
object Day11Part2 {

  final case class Holes(rows: List[Int], columns: List[Int])

  final case class Point(x: Int, y: Int)

  def getEmptyRowsAndColumns(matrix: Vector[Vector[Boolean]]): Holes = {
    val emptyRows = matrix.zipWithIndex.filter(_._1.forall(_ == false)).map(_._2).toList
    val emptyColumns = matrix.flatMap(_.zipWithIndex).groupMapReduce(_._2)(_._1)((a, b) => a || b)
      .filter(!_._2).keys.toList.sorted

    Holes(emptyRows, emptyColumns)
  }

  def getAllTuples(matrix: Vector[Vector[Boolean]]): List[(Point, Point)] = {
    val points = matrix.zipWithIndex
      .flatMap((vector, y) => vector.zipWithIndex.filter(_._1).map((_, x) => Point(x, y)))
      .toList

    @tailrec
    def inner(remaining: List[Point], acc: List[(Point, Point)]): List[(Point, Point)] =
      remaining match
        case Nil => acc
        case head :: tail => inner(tail, tail.map(e => (head, e)) ++ acc)

    inner(points, List())
  }

  def getPathLength(a: Point, b: Point, holes: Holes, multiplier: Long): Long = {
    val maxX = Math.max(a.x, b.x)
    val minX = Math.min(a.x, b.x)

    val maxY = Math.max(a.y, b.y)
    val minY = Math.min(a.y, b.y)

    val holesX = holes.columns.count(c => minX < c && c < maxX)
    val addX = holesX * multiplier - holesX
    val holesY = holes.rows.count(c => minY < c && c < maxY)
    val addY = holesY * multiplier - holesY

    maxX + addX - minX + maxY + addY - minY
  }

  private def getAllPathsLengths(points: Seq[(Point, Point)], holes: Holes, multiplier: Long): Long =
    points.map(p => getPathLength(p._1, p._2, holes, multiplier)).sum

  def parse(all: String): Vector[Vector[Boolean]] =
    Source.fromString(all).getLines().map(line => line.map(_ != '.').toVector).toVector

  def check(all: String, multiplier: Long): Long = {
    val matrix = parse(all)
    val holes = getEmptyRowsAndColumns(matrix)
    getAllPathsLengths(getAllTuples(matrix), holes, multiplier)
  }
}

