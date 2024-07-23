package p23.day6

import scala.io.Source
import scala.jdk.StreamConverters.*

object Day6Part1 {

  private final case class Race(time: Int, distanceToBeat: Int)

  private def parseRaces(lines: String): Seq[Race] = {
    Source.fromString(lines).getLines().toList match
      case head :: List(last) =>
        (head, last) match
          case (s"Time:$times", s"Distance:$distances") =>
            times.split(" ").flatMap(_.trim.toIntOption)
              .zip(distances.split(" ").flatMap(_.trim.toIntOption))
              .map((t, d) => Race(t, d))
      case _ => Seq()
  }

  private def getCombinationOfMoves(time: Int): Seq[Int] =
    Range(0, time).map(v => {
      val distance = (time - v) * v
      distance
    })

  private def nbOfWaysToBeatTheRecord(distanceToBeat: Int, time: Int): Int =
    getCombinationOfMoves(time).count(d => d > distanceToBeat)

  def check(all: String): Int =
    parseRaces(all).map(race => nbOfWaysToBeatTheRecord(race.distanceToBeat, race.time)).product
}
