package p23.day11

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.io.Source

//noinspection DuplicatedCode
object Day11Part1 {

  final case class Point(x: Int, y: Int)

  /**
   * x ->
   * y
   */
  def expandMatrix(matrix: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] = {
    val emptyRows = matrix.zipWithIndex.filter(_._1.forall(_ == false)).map(_._2).toList
    val emptyColumns = matrix.flatMap(_.zipWithIndex).groupMapReduce(_._2)(_._1)((a, b) => a || b)
      .filter(!_._2).keys.toList.sorted

    @tailrec
    def innerRow(emptyRows: List[Int], matrix: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] = {
      emptyRows match
        case Nil => matrix
        case head :: tail =>
          innerRow(tail.map(_ + 1), matrix.take(head) ++ Vector(matrix(head)) ++ matrix.drop(head))
    }

    @tailrec
    def innerColumns(emptyColumns: List[Int], matrix: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] = {
      emptyColumns match
        case Nil => matrix
        case head :: tail =>
          val newMatrix = matrix.map(v => {
            v.take(head) ++ Vector(v(head)) ++ v.drop(head)
          })
          innerColumns(tail.map(_ + 1), newMatrix)
    }

    innerColumns(emptyColumns, innerRow(emptyRows, matrix))
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

  def getPathLength(a: Point, b: Point): Int =
    Math.max(a.x, b.x) - Math.min(a.x, b.x) + Math.max(a.y, b.y) - Math.min(a.y, b.y)

  private def getAllPathsLengths(points: Seq[(Point, Point)]): Int = {
    points.map(p => getPathLength(p._1, p._2)).sum
  }

  def parse(all: String): Vector[Vector[Boolean]] =
    Source.fromString(all).getLines().map(line => line.map(_ != '.').toVector).toVector

  def toString(matrix: Vector[Vector[Boolean]]): String = {
    matrix.map(line => line.map {
      case false => '.'
      case _ => '#'
    }.mkString).mkString("\n")
  }

  def check(all: String): Int =
    getAllPathsLengths(getAllTuples(expandMatrix(parse(all))))
}

