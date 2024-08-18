package p23.day09

import scala.annotation.tailrec
import scala.io.Source

//noinspection DuplicatedCode
object Day9Part1 {

  private def computeNewLine(line: List[Long]): List[Long] =
    line.sliding(2).map(_.reduceLeft((a, b) => b - a)).toList

  private def allLines(start: List[Long]): List[List[Long]] = {
    @tailrec
    def inner(acc: List[List[Long]]): List[List[Long]] =
      acc match
        case _ :+ last if last.forall(_ == 0) => acc
        case _ :+ last =>
          inner(acc.appended(computeNewLine(last)))

    inner(List(start))
  }

  private def getSum(lines: List[List[Long]]): Long =
    lines.map(_.last).sum

  private def parseLines(all: String): List[List[Long]] =
    Source.fromString(all).getLines().map(_.split(" ").map(_.toLong).toList).toList

  def check(all: String): Long =
    parseLines(all).map(l => getSum(allLines(l))).sum
}

