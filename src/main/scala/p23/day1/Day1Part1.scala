package p23.day1

import scala.util.matching.Regex

object Day1Part1 {

  private val numberPattern: Regex = "([0-9])".r

  def getLineSumIfAny(line: String): Long = {
    numberPattern.findAllIn(line).toSeq match
      case head :: Nil => (head + head).toLong
      case head :: tail => (head + tail.last).toLong
      case _ => 0
  }

  def getSum(lines: Seq[String]): Long =
    lines.map(getLineSumIfAny).sum
}
