package p23.day05

import org.scalatest.funsuite.AnyFunSuite
import p23.day05.Day5Part2.{FromTo, Interval}

import scala.io.Source

class Day5Part2Test extends AnyFunSuite {

  // Left

  // ---
  //  ---
  test("Check case 1") {
    val seed = Interval(1, 3)
    val fromTo = FromTo(Interval(2, 4), Interval(14, 16))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.contains(Interval(1, 1)))
    assert(generated.contains(Interval(14, 15)))
  }

  // --
  //  --
  test("Check case 2") {
    val seed = Interval(1, 2)
    val fromTo = FromTo(Interval(2, 3), Interval(14, 15))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.contains(Interval(1, 1)))
    assert(generated.contains(Interval(14, 14)))
  }

  // -
  // --
  test("Check case 3") {
    val seed = Interval(2, 2)
    val fromTo = FromTo(Interval(2, 3), Interval(14, 15))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.isEmpty)
    assert(generated.contains(Interval(14, 14)))
  }

  // --
  //   --
  test("Check case 4") {
    val seed = Interval(2, 3)
    val fromTo = FromTo(Interval(4, 5), Interval(14, 15))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.contains(seed))
    assert(generated.isEmpty)
  }

  // Right

  //   ---
  //  ---
  test("Check case 7") {
    val seed = Interval(1, 3)
    val fromTo = FromTo(Interval(0, 2), Interval(12, 14))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.contains(Interval(3, 3)))
    assert(generated.contains(Interval(13, 14)))
  }

  //  --
  // --
  test("Check case 5") {
    val seed = Interval(1, 2)
    val fromTo = FromTo(Interval(0, 1), Interval(12, 13))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.contains(Interval(2, 2)))
    assert(generated.contains(Interval(13, 13)))
  }

  //  -
  // --
  test("Check case 6") {
    val seed = Interval(1, 1)
    val fromTo = FromTo(Interval(0, 1), Interval(12, 13))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.isEmpty)
    assert(generated.contains(Interval(13, 13)))
  }

  //   --
  // --
  test("Check case 8") {
    val seed = Interval(2, 3)
    val fromTo = FromTo(Interval(0, 1), Interval(12, 13))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.contains(Interval(2, 3)))
    assert(generated.isEmpty)
  }

  // Other

  // -
  // -
  test("Check case 11") {
    val seed = Interval(1, 1)
    val fromTo = FromTo(Interval(1, 1), Interval(2, 2))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.isEmpty)
    assert(generated.contains(Interval(2, 2)))
  }

  // --
  // --
  test("Check case 11333") {
    val seed = Interval(1, 2)
    val fromTo = FromTo(Interval(1, 2), Interval(2, 3))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.isEmpty)
    assert(generated.contains(Interval(2, 3)))
  }

  // ----
  //  --
  test("Check case 10") {
    val seed = Interval(0, 4)
    val fromTo = FromTo(Interval(1, 2), Interval(2, 3))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced == Seq(Interval(0, 0), Interval(3, 4)))
    assert(generated.contains(Interval(2, 3)))
  }

  //  --
  // ----
  test("Check case 9") {
    val seed = Interval(1, 2)
    val fromTo = FromTo(Interval(0, 4), Interval(10, 14))
    val (reduced, generated) = Day5Part2.getIntervalIntersection(seed, fromTo)
    assert(reduced.isEmpty)
    assert(generated.contains(Interval(11, 12)))
  }

  test("Check example 1") {
    val lines =
      """
        |seeds: 11 1
        |
        |seed-to-soil map:
        |0 10 1
        |""".stripMargin.trim

    assert(Day5Part2.check(Source.fromString(lines).mkString) == 11)
  }

  test("Check example 1zefzef") {
    val lines =
      """
        |seeds: 11 1
        |
        |seed-to-soil map:
        |0 11 1
        |""".stripMargin.trim

    assert(Day5Part2.check(Source.fromString(lines).mkString) == 0)
  }

  test("Check example 1zefzzfzefef") {
    val lines =
      """
        |seeds: 1 5
        |
        |seed-to-soil map:
        |0 10 6
        |""".stripMargin.trim

    assert(Day5Part2.check(Source.fromString(lines).mkString) == 1)
  }

  test("Check example 1zefzzfzzezfefef") {
    val lines =
      """
        |seeds: 12 5
        |
        |seed-to-soil map:
        |0 10 6
        |""".stripMargin.trim

    assert(Day5Part2.check(Source.fromString(lines).mkString) == 2)
  }

  test("Check example 1zefzzfzzezfefzefef") {
    val lines =
      """
        |seeds: 12 5
        |
        |seed-to-soil map:
        |0 10 6
        |seed-to-dazd map:
        |5 2 1
        |""".stripMargin.trim

    assert(Day5Part2.check(Source.fromString(lines).mkString) == 5)
  }

  test("Check example 2") {
    val lines =
      """
        |seeds: 79 14 55 13
        |
        |seed-to-soil map:
        |50 98 2
        |52 50 48
        |""".stripMargin.trim

    assert(Day5Part2.check(Source.fromString(lines).mkString) == 57)
  }

  test("Check example 3") {
    val lines =
      """
        |seeds: 79 14 55 13
        |
        |seed-to-soil map:
        |50 98 2
        |52 50 48
        |
        |soil-to-fertilizer map:
        |0 15 37
        |37 52 2
        |39 0 15
        |
        |fertilizer-to-water map:
        |49 53 8
        |0 11 42
        |42 0 7
        |57 7 4
        |
        |water-to-light map:
        |88 18 7
        |18 25 70
        |
        |light-to-temperature map:
        |45 77 23
        |81 45 19
        |68 64 13
        |
        |temperature-to-humidity map:
        |0 69 1
        |1 0 69
        |
        |humidity-to-location map:
        |60 56 37
        |56 93 4
        |""".stripMargin.trim

    assert(Day5Part2.check(Source.fromString(lines).mkString) == 46)
  }
}
