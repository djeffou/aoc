package p23.day12

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.Source

//noinspection DuplicatedCode
object Day12Part2 {

  final case class Pattern(size: Int, list: List[List[Char]])

  final case class Line(pattern: String, nb: List[Int])

  def parse(line: String): Line = line match
    case s"$pattern $nbs" =>
      Line(
        Range(0, 5).map(_ => pattern).mkString("?"),
        Range(0, 5).map(_ => nbs).mkString(",").split(',').toList.map(_.toInt)
      )

  def function(pattern: List[Char], numbers: List[Int]): Int = {

    (pattern, numbers) match
      case (Nil, _) => numbers.isEmpty
      case (_, Nil) => !pattern.contains('#')
      case (headChar :: tailChar, _) if headChar == '.' => function(tailChar, numbers)
      case (headChar :: tailChar, headNumbers :: tailNumbers) if headChar == '?' =>
        function('#' +: tailChar, numbers) + function('.' +: tailChar, numbers)
      case (headChar :: tailChar, headNumbers :: tailNumbers) if headChar == '#' =>


    1
  }

  def check(all: String): Long = {
    val start = System.currentTimeMillis()
    val lines = Source.fromString(all).getLines().map(parse).toSeq
    1L
  }
}

