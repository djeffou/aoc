package p23.day12

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.Source

//noinspection DuplicatedCode
object Day12Part1 {

  final case class Pattern(size: Int, list: List[List[Char]])

  final case class Line(pattern: String, nb: List[Int])

  private def isCorrect(line: Line): Boolean = {
    val splits = line.pattern.trim.split("(\\.)+").filter(_.nonEmpty)
    splits.length == line.nb.size && splits.map(_.length).zip(line.nb).forall((a, b) => a == b)
  }

  private def getAllPatternsBySize(elements: List[Char], list: Seq[Line]): Seq[Pattern] = {
    val counts = list.map(_.pattern.count(_ == '?'))
    Range.inclusive(counts.min, counts.max).map(nb => Pattern(nb, getPatternBySize(elements, nb)))
  }

  private def getPatternBySize(elements: List[Char], size: Int): List[List[Char]] = {
    @tailrec
    def inner(remainingSize: Int, acc: List[List[Char]]): List[List[Char]] = {
      if (remainingSize > 0)
        inner(remainingSize - 1, acc.flatMap(l => elements.map(c => c +: l)))
      else acc
    }

    inner(size, List(List()))
  }

  private def getCombinations(line: Line, patterns: Seq[Pattern]): List[Line] = {
    val count = line.pattern.count(_ == '?')
    val combinations = patterns.find(_.size == count).get.list

    val groups = line.pattern.split("\\?", -1)
    combinations.map(c => {
      groups.zip(c.padTo(groups.length, "")).map((s, c) => s + c).mkString
    }).map(Line(_, line.nb))
  }

  def parse(line: String): Line = line match
    case s"$pattern $nbs" => Line(pattern, nb = nbs.split(',').toList.map(_.toInt))

  def check(all: String): Long = {
    val start = System.currentTimeMillis()
    val lines = Source.fromString(all).getLines().map(parse).toSeq
    val patterns = getAllPatternsBySize(List('#', '.'), lines)

    lines.par.flatMap(line => getCombinations(line, patterns))
      .count(isCorrect)
  }
}

