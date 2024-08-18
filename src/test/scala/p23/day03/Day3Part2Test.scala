package p23.day03

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day3Part2Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |..35..633.
        |......#...
        |617*......
        |.....+.58.
        |..592.....
        |......755.
        |...$.*....
        |""".stripMargin.trim
    assert(Day3Part2.check(Source.fromString(lines).getLines().toSeq) == 0)
  }

  test("Check example 2") {
    val lines =
      """
        |467..114..
        |...*......
        |..35..633.
        |""".stripMargin.trim
    assert(Day3Part2.check(Source.fromString(lines).getLines().toSeq) == 16345)
  }

  test("Check example 3") {
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
    assert(Day3Part2.check(Source.fromString(lines).getLines().toSeq) == 467835)
  }

  test("Compute real case") {
    val result = Day3Part2.check(Source.fromResource("p23/day3/input.txt").getLines().toSeq)
    assert(result == 79026871)
  }
}
