package p23.day3

import java.util.UUID
import scala.util.Try
import scala.util.matching.Regex

object Day3Part2 {

  private val numberPattern: Regex = "([0-9]*)".r

  private type Matrix = Array[Array[Char]]

  private final case class Position(row: Int, column: Int)

  private final case class Element(char: Char, position: Position)

  private final case class Group(elements: Seq[Element])

  private final case class GroupWithSymbolPositions(group: Group, symbols: Set[Position], id: UUID = UUID.randomUUID())

  private def isSymbol(e: Element): Boolean = e.char == '*'

  private def getValue(a: Matrix, position: Position): Option[Element] =
    Try(a(position.row)(position.column)).toOption.map(c => Element(c, position))

  private def getAllValuesAround(a: Matrix, position: Position): Seq[Element] =
    Seq(
      getValue(a, position.copy(row = position.row - 1)),
      getValue(a, position.copy(row = position.row - 1, column = position.column - 1)),
      getValue(a, position.copy(row = position.row - 1, column = position.column + 1)),
      getValue(a, position.copy(row = position.row + 1)),
      getValue(a, position.copy(row = position.row + 1, column = position.column - 1)),
      getValue(a, position.copy(row = position.row + 1, column = position.column + 1)),
      getValue(a, position.copy(column = position.column - 1)),
      getValue(a, position.copy(column = position.column + 1))
    ).flatten

  private def getSymbolsAround(a: Matrix, position: Position): Seq[Position] =
    getAllValuesAround(a, position).filter(isSymbol).map(_.position)

  private def parseFile(lines: Seq[String]): Matrix = lines.map(_.toArray).toArray

  private def getGroups(lines: Seq[String]): Seq[Group] =
    lines.zipWithIndex.flatMap((line, row) => {
      numberPattern.findAllMatchIn(line).map(m => {
        Group(m.group(1).zipWithIndex.map((char, index) => Element(char, Position(row, m.start + index))))
      })
    })

  private def getGroupsWithSymbols(a: Matrix, groups: Seq[Group]): Seq[GroupWithSymbolPositions] =
    groups.map(group => {
      val symbols = group.elements.flatMap(e => getSymbolsAround(a, e.position))
      GroupWithSymbolPositions(group, symbols.toSet)
    })

  private def clusterGroupsOfTwo(groups: Seq[GroupWithSymbolPositions]): Iterable[Seq[GroupWithSymbolPositions]] =
    groups
      .flatMap(g => g.symbols.map(p => (p, g)))
      .groupMap(_._1)(_._2)
      .map {
        case (position, value) => (position, value.distinctBy(_.id))
      }.filter(_._2.size == 2)
      .values

  def check(lines: Seq[String]): Long = {
    val matrix = parseFile(lines)

    val groups = getGroupsWithSymbols(matrix, getGroups(lines))
      .filter(_.symbols.nonEmpty)

    clusterGroupsOfTwo(groups)
      .map(cluster => cluster.map(_.group.elements.map(_.char).mkString).map(_.toLong).product)
      .sum
  }

}
