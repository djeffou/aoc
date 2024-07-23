package p23.day10

import scala.annotation.tailrec
import scala.io.Source

//noinspection DuplicatedCode
object Day10Part2 {

  private final case class Loop(steps: Int, path: List[(Int, Int)])

  private enum Direction:
    case North extends Direction
    case South extends Direction
    case East extends Direction
    case West extends Direction
    case Stop extends Direction
    case End extends Direction

  private def getNewPosition(x: Int, y: Int, direction: Direction): (Int, Int) =
    direction match
      case Direction.North => (x, y - 1)
      case Direction.South => (x, y + 1)
      case Direction.East => (x + 1, y)
      case Direction.West => (x - 1, y)
      case _ => (-1, -1)

  private def getDirection(goingTo: Direction, symbol: Char): Direction =
    symbol match
      case '|' if goingTo == Direction.North => Direction.North
      case '|' if goingTo == Direction.South => Direction.South

      case '-' if goingTo == Direction.West => Direction.West
      case '-' if goingTo == Direction.East => Direction.East

      case 'L' if goingTo == Direction.West => Direction.North
      case 'L' if goingTo == Direction.South => Direction.East

      case 'J' if goingTo == Direction.South => Direction.West
      case 'J' if goingTo == Direction.East => Direction.North

      case '7' if goingTo == Direction.East => Direction.South
      case '7' if goingTo == Direction.North => Direction.West

      case 'F' if goingTo == Direction.North => Direction.East
      case 'F' if goingTo == Direction.West => Direction.South

      case 'S' => Direction.End

      case _ => Direction.Stop

  private def getMaxLoop(all: Vector[Vector[Char]], start: (Int, Int)): Option[Loop] = {
    val yMax = all.size - 1
    val xMax = all.map(_.size).max - 1

    @tailrec
    def inner(position: (Int, Int), goingTo: Direction, loop: Loop): Option[Loop] = {
      position match
        case (x, y) if goingTo == Direction.End => Some(loop)
        case (x, y) if x < 0 || x > xMax || y < 0 || y > yMax || goingTo == Direction.Stop => None
        case (x, y) =>
          val newDirection = getDirection(goingTo, all(y)(x))
          val newPosition = getNewPosition(x, y, newDirection)
          inner(newPosition, newDirection, Loop(loop.steps + 1, loop.path.appended((x, y))))
    }

    List(
      Direction.North,
      Direction.South,
      Direction.East,
      Direction.West,
    ).flatMap(d => inner(getNewPosition(start._1, start._2, Direction.East), Direction.East, Loop(0, List())))
      .maxByOption(_.steps)
  }

  // https://en.wikipedia.org/wiki/Shoelace_formula#Example
  private def computeInsidePoints(areaPath: List[(Int, Int)]): Int = {
    val area = (areaPath :+ areaPath.head).sliding(2).map {
      case Seq((xA, yA), (xB, yB)) =>
        (xA * yB) - (xB * yA)
    }.sum.abs
    (area + 2 - areaPath.size) / 2
  }

  private def findStart(all: Vector[Vector[Char]]): (Int, Int) =
    all.zipWithIndex.flatMap((line, y) => {
      line.zipWithIndex.find((c, x) => c == 'S').map((_, x) => (x, y))
    }).head

  /**
   * x ->
   * y
   */
  private def parse(all: String): Vector[Vector[Char]] =
    Source.fromString(all.trim).getLines().map(_.trim.toVector).toVector

  def check(all: String): Double = {
    val v = parse(all)
    getMaxLoop(v, findStart(v)) match
      case Some(value) =>
        computeInsidePoints(value.path)
      case None => -1
  }
}

