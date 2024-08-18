package p23.day03

import org.scalatest.funsuite.AnyFunSuite
import p23.day01.Day1Part1

import scala.io.Source

class Day3Part1Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |467..114..
        |...*......
        |""".stripMargin.trim
    assert(Day3Part1.check(Source.fromString(lines).getLines().toSeq) == 467)
  }

  test("Check example 2") {
    val lines =
      """
        |467..114..
        |...*......
        |..35..633.
        |......#...
        |617*......
        |.....+.58.
        |..592.....
        |......755.
        |...$.*....
        |.664.598..
        |""".stripMargin.trim
    assert(Day3Part1.check(Source.fromString(lines).getLines().toSeq) == 4361)
  }

  test("Compute real case") {
    val result = Day3Part1.check(Source.fromResource("p23/day3/input.txt").getLines().toSeq)
    assert(result == 527364)
  }
}
