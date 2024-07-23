package p23.day6

import scala.collection.parallel.immutable.ParRange
import scala.io.Source

object Day6Part2 {

  private final case class Race(time: Int, distanceToBeat: Long)

  private def parseRaces(lines: String): Option[Race] = {
    Source.fromString(lines).getLines().toList match
      case head :: List(last) =>
        (head, last) match
          case (s"Time:$times", s"Distance:$distances") =>
            times.split(" ").map(_.trim).mkString.toIntOption.flatMap(t => {
              distances.split(" ").flatMap(_.trim).mkString.toLongOption.map(d => Race(t, d))
            })
          case _ => None
      case _ => None
  }

  private def getCount(distanceToBeat: Long, time: Int): Long =
    ParRange(0, time, 1, false).map(v =>
      (time.toLong - v.toLong) * v.toLong
    ).count(d => d > distanceToBeat)

  def check(all: String): Long =
    parseRaces(all).map(race => getCount(race.distanceToBeat, race.time)).getOrElse(-1)
}
