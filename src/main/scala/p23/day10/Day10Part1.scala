package p23.day10

import scala.annotation.tailrec
import scala.io.Source

//noinspection DuplicatedCode
object Day10Part1 {

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

  private def getMaxLoop(all: Vector[Vector[Char]], start: (Int, Int)): Option[Long] = {
    val yMax = all.size - 1
    val xMax = all.map(_.size).max - 1

    @tailrec
    def inner(position: (Int, Int), goingTo: Direction, steps: Int): Option[Long] =
      position match
        case (x, y) if goingTo == Direction.End => Some(steps)
        case (x, y) if goingTo == Direction.Stop => None
        case (x, y) if x < 0 || x > xMax || y < 0 || y > yMax => None
        case (x, y) =>
          val newDirection = getDirection(goingTo, all(y)(x))
          val newPosition = getNewPosition(x, y, newDirection)
          inner(newPosition, newDirection, steps + 1)

    List(
      Direction.North,
      Direction.South,
      Direction.East,
      Direction.West,
    ).flatMap(d => inner(getNewPosition(start._1, start._2, d), d, 0)).maxOption
  }

  private def findStart(all: Vector[Vector[Char]]): (Int, Int) =
    all.zipWithIndex.flatMap((line, y) => {
      line.zipWithIndex.find((c, x) => c == 'S').map((_, x) => (x, y))
    }).head

  /**
   * x
   * y
   */
  private def parse(all: String): Vector[Vector[Char]] =
    Source.fromString(all.trim).getLines().map(_.trim.toVector).toVector

  def check(all: String): Long = {
    val v = parse(all)
    getMaxLoop(v, findStart(v)).map(_ / 2).getOrElse(-1)
  }
}

