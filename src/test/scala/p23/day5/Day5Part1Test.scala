package p23.day5

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day5Part1Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |seeds: 79
        |
        |seed-to-soil map:
        |50 98 2
        |52 50 48
        |""".stripMargin.trim

    assert(Day5Part1.check(Source.fromString(lines).mkString) == 81)
  }

  test("Check example 2") {
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

    assert(Day5Part1.check(Source.fromString(lines).mkString) == 35)
  }

  test("Compute real case") {
    val result = Day5Part1.check(Source.fromResource("p23/day5/input.txt").mkString)
    assert(result == 88151870)
  }
}
