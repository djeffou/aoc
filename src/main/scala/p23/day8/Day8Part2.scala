package p23.day8

import scala.annotation.tailrec
import scala.io.Source

//noinspection DuplicatedCode
object Day8Part2 {

  private final case class Line(id: String, left: String, right: String)

  private def parseLine(line: String): Option[Line] =
    line match
      case s"$first = ($second, $third)" => Some(Line(first.trim, second.trim, third.trim))
      case _ => None

  private def parseLines(all: String): (List[Char], List[Line]) =
    Source.fromString(all).getLines().toList match
      case head :: tail =>
        (head.trim.toCharArray.toList, tail.flatMap(parseLine))
      case _ => (List(), List())

  private def getPathToEnd(instructions: List[Char], line: Line, lines: List[Line], index: Map[String, Line]): Option[Long] = {
    @tailrec
    def inner(remainingInstructions: List[Char], id: String, steps: Long): Option[Long] =
      (remainingInstructions, index.get(id)) match
        case (_, _) if id.endsWith("Z") => Some(steps)
        case (Nil, _) => inner(instructions, id, steps)
        case (instruction :: tail, Some(i)) if instruction == 'L' =>
          inner(tail, i.left, steps + 1)
        case (instruction :: tail, Some(i)) if instruction == 'R' =>
          inner(tail, i.right, steps + 1)
        case _ => None

    inner(instructions, line.id, 0)
  }

  private def lcm(list: List[Long]): Long = list.foldLeft(1: Long) {
    (a, b) =>
      b * a / LazyList.iterate((a, b)) {
        case (x, y) => (y, x % y)
      }.dropWhile(_._2 != 0).head._1.abs
  }

  private def getStepsNb(instructions: List[Char], lines: List[Line]): Option[Long] = {
    val index = lines.groupBy(_.id).view.mapValues(_.head).toMap
    val nb = lines.filter(_.id.endsWith("A")).flatMap(getPathToEnd(instructions, _, lines, index))
    println(nb)
    Some(lcm(nb))
  }

  def check(all: String): Long = {
    val (instructions, lines) = parseLines(all)
    getStepsNb(instructions, lines).getOrElse(-1)
  }
}