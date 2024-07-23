package p23.day5

import scala.annotation.tailrec
import scala.jdk.StreamConverters.*
import scala.util.Try

//noinspection DuplicatedCode
object Day5Part2 {

  final case class Interval(min: Long, max: Long)

  final case class FromTo(from: Interval, to: Interval)

  private final case class All(seeds: List[Interval], groups: List[List[FromTo]])

  private def parseAll(all: String): Option[All] = Try {
    val split = all.split("map:")
    val seeds = parseSeeds(split.head)
    val ranges = split.drop(1).map(_.lines().toScala(List).flatMap(parseLine)).toList
    All(seeds, ranges)
  }.toOption

  private def parseSeeds(s: String): List[Interval] = {
    val nb = s.lines().toScala(List).mkString(" ").split(" ").flatMap(_.toLongOption).toList
    nb.sliding(2, 2).flatMap {
      case List(a, b) =>
        Some(Interval(a, a + b - 1L))
      case _ => None
    }.toList
  }

  private def parseLine(line: String): Option[FromTo] = Try {
    line match
      case s"$destination $start $increment" =>
        val from = Interval(start.toLong, start.toLong + increment.toLong - 1)
        val to = Interval(destination.toLong, destination.toLong + increment.toLong - 1)
        Some(FromTo(from, to))
      case _ => None
  }.toOption.flatten

  def getIntervalIntersection(seed: Interval, fromTo: FromTo): (List[Interval], List[Interval]) = {
    (seed, fromTo.from) match
      case (a, b) if a == b =>
        (List(), List(Interval(fromTo.to.min, fromTo.to.max)))

      case (Interval(aMin, aMax), Interval(bMin, bMax)) if aMin <= bMin && bMin <= aMax && aMax <= bMax =>
        val reducedSeed = if (aMin == aMax) {
          List()
        } else {
          List(Interval(aMin, List(aMin, aMax, bMin).min))
        }
        val generatedSeed = Interval(fromTo.to.min, fromTo.to.max - (bMax - aMax))
        if (reducedSeed.exists(_.max == -1877401874))
          println("PLOP")
        (reducedSeed, List(generatedSeed))

      case (Interval(aMin, aMax), Interval(bMin, bMax)) if bMin <= aMin && aMin <= bMax && bMax <= aMax =>
        val reducedSeed = if (aMin == aMax) {
          List()
        } else {
          List(Interval(List(aMin, aMax, bMin).max, aMax))
        }
        val generatedSeed = Interval(fromTo.to.min + (aMin - bMin), fromTo.to.max)
        if (reducedSeed.exists(_.max == -1877401874))
          println("PLOP")
        (reducedSeed, List(generatedSeed))

      case (Interval(aMin, aMax), Interval(bMin, bMax)) if aMax < bMin || aMin > bMax =>

        val reducedSeed = List(seed)
        if (reducedSeed.exists(_.max == -1877401874))
          //println(s"PLIP: $aMin $aMax $bMin $bMax")
          println()
        (List(seed), List())

      case (Interval(aMin, aMax), Interval(bMin, bMax)) if bMin < aMin && aMax < bMax =>
        (List(), List(Interval((aMin - bMin) + fromTo.to.min, fromTo.to.max - (bMax - aMax))))


      case (Interval(aMin, aMax), Interval(bMin, bMax)) if aMin < bMin && aMax > bMax =>
        val reducedSeed = List(Interval(aMin, bMin - 1), Interval(bMax + 1, aMax))
        if (reducedSeed.exists(_.max == -1877401874))
          println(s"YOO: $aMin $aMax $bMin $bMax")
          println()
        (reducedSeed, List(Interval(fromTo.to.min, fromTo.to.max)))

      case _ =>
        println("PLOP")
        (List(), List())
  }


  private def getNewSeedsForAGroup(seed: Interval, group: List[FromTo]): List[Interval] = {
    @tailrec
    def inner(seed: List[Interval], group: List[FromTo], acc: List[Interval]): List[Interval] = {
      //      println("Start")
      //      println(s"S: $seed")
      //      println(s"G (${group.size}: $group")
      //      println(s"I: $acc")
      //println()
      (seed, group) match
        case (Nil, _) => acc
        case (headSeed :: tailSeed, head :: tail) =>
          val a: (List[Interval], List[Interval]) = getIntervalIntersection(headSeed, head)
          if (zeof(a._1)) {
            //println("S: " + headSeed)
            //getIntervalIntersection(headSeed, head)
            //println("H: " + head)
            //println()
          }
          inner((a._1 ++ tailSeed).distinct, tail, acc.prependedAll(a._2))
        case (seeds, Nil) =>
          acc ++ seeds
    }

    inner(List(seed), group, List())
  }

  private def zeof(s: List[Interval]): Boolean = {
    s.exists(s => {
      //println(s)
      val a = s.max - s.min
      a < 0
    })
  }

  private def print(s: List[Interval]): Unit = {
    val b = s.map(s => {
      //println(s)
      val a = s.max - s.min
      a
    }).sum
    //println(b)
  }

  @tailrec
  private def getFinalIntervalForSeed(initialSeeds: List[Interval], groups: List[List[FromTo]]): List[Interval] = {
    println("Init seeds: " + initialSeeds)
    //println("Groups: " + groups)
    groups match
      case head :: tail =>
        val a = initialSeeds.flatMap(getNewSeedsForAGroup(_, head))
        //print(a)
        getFinalIntervalForSeed(a, tail)
      case Nil => initialSeeds
  }

  def check(all: String): Long = {
    parseAll(all)
      .toList
      .flatMap(a => getFinalIntervalForSeed(a.seeds, a.groups))
      .map(_.min)
      .min
  }
}
