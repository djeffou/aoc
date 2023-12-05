package p23.day3

import scala.util.Try
import scala.util.matching.Regex

object Day3Part1 {

  private val numberPattern: Regex = "([0-9]*)".r

  private final case class Element(char: Char, row: Int, column: Int)

  private final case class Group(elements: Seq[Element])

  private def isSymbol(s: Char): Boolean = s.toString.toLongOption.isEmpty && s != '.'

  private def getValue(a: Array[Array[Char]], row: Int, column: Int): Option[Char] =
    Try(a(row)(column)).toOption

  private def getAllValuesAround(a: Array[Array[Char]], row: Int, column: Int): Seq[Option[Char]] =
    Seq(
      getValue(a, row - 1, column),
      getValue(a, row - 1, column - 1),
      getValue(a, row - 1, column + 1),
      getValue(a, row + 1, column),
      getValue(a, row + 1, column - 1),
      getValue(a, row + 1, column + 1),
      getValue(a, row, column - 1),
      getValue(a, row, column + 1)
    )

  private def isSymbolAround(a: Array[Array[Char]], row: Int, column: Int): Boolean =
    getAllValuesAround(a, row, column).map(e => e.map(isSymbol)).exists(e => e.getOrElse(false))

  private def parseFile(lines: Seq[String]): Array[Array[Char]] = lines.map(_.toArray).toArray

  def check(lines: Seq[String]): Long = {
    val matrix = parseFile(lines)

    val groups: Seq[Group] = lines.zipWithIndex.flatMap((line, row) => {
      numberPattern.findAllMatchIn(line).map(m => {
        Group(m.group(1).zipWithIndex.map((char, index) => Element(char, row, m.start + index)))
      })
    })

    groups.filter(_.elements.exists(e => isSymbolAround(matrix, e.row, e.column)))
      .flatMap(group => group.elements.map(_.char).mkString.toLongOption)
      .sum
  }


}
