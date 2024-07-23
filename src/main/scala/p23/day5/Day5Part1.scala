package p23.day5

import scala.annotation.tailrec
import scala.jdk.StreamConverters.*
import scala.util.Try

//noinspection DuplicatedCode
object Day5Part1 {

  private final case class Range(start: Long, end: Long, destination: Long)

  private final case class All(seeds: List[Int], ranges: List[List[Range]])

  private def parseAll(all: String): Option[All] = Try {
    val split = all.split("map:")
    val seeds = split.head.lines().toScala(List).mkString(" ").split(" ").flatMap(_.toIntOption).toList
    val ranges = split.drop(1).map(_.lines().toScala(List).flatMap(parseLine)).toList
    All(seeds, ranges)
  }.toOption

  private def parseLine(line: String): Option[Range] = Try {
    line match
      case s"$destination $start $increment" =>
        Some(Range(start.toLong, start.toLong + increment.toLong, destination.toLong))
      case _ => None
  }.toOption.flatten

  private def getDestinationOrDefault(seed: Long, ranges: Seq[Range]): Long =
    ranges.find(range => range.start <= seed && seed <= range.end)
      .map(range => range.destination + seed - range.start)
      .getOrElse(seed)

  @tailrec
  private def getRecipe(seed: Long, allSteps: List[List[Range]]): Long =
    allSteps match
      case head :: tail =>
        getRecipe(getDestinationOrDefault(seed, head), tail)
      case _ =>
        seed

  def check(all: String): Long =
    parseAll(all).map(a =>
      a.seeds.map(getRecipe(_, a.ranges)).min
    ).getOrElse(0)
}
