package p23.day8

import scala.annotation.tailrec
import scala.io.Source

//noinspection DuplicatedCode
object Day8Part1 {

  final case class Line(id: String, left: String, right: String)

  def parseLine(line: String): Option[Line] =
    line match
      case s"$first = ($second, $third)" => Some(Line(first.trim, second.trim, third.trim))
      case _ => None

  def parseLines(all: String): (List[Char], List[Line]) =
    Source.fromString(all).getLines().toList match
      case head :: tail =>
        (head.trim.toCharArray.toList, tail.flatMap(parseLine))
      case _ => (List(), List())

  def getStepsNb(instructions: List[Char], lines: List[Line]): Option[Int] = {
    val index = lines.groupBy(_.id).view.mapValues(_.head)

    @tailrec
    def inner(remainingInstructions: List[Char], id: String, steps: Int): Option[Int] = {
      (id, remainingInstructions) match
        case ("ZZZ", _) => Some(steps)
        case (_, Nil) => inner(instructions, id, steps)
        case (i, instruction :: tail) =>
          index.get(i) match
            case Some(value) if instruction == 'L' => inner(tail, value.left, steps + 1)
            case Some(value) if instruction == 'R' => inner(tail, value.right, steps + 1)
            case _ => None
    }

    (instructions, index.get("AAA")) match
      case (headI :: tailI, Some(v)) if headI == 'L' => inner(tailI, v.left, 1)
      case (headI :: tailI, Some(v)) if headI == 'R' => inner(tailI, v.right, 1)
      case _ => None
  }

  def check(all: String): Int = {
    val (instructions, lines) = parseLines(all)
    getStepsNb(instructions, lines).getOrElse(-1)
  }
}

