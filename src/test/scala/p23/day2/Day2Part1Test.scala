package p23.day2

import org.scalatest.funsuite.AnyFunSuite
import p23.day1.Day1Part1

import scala.io.Source

class Day2Part1Test extends AnyFunSuite {

  private val max = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14
  )

  test("Check example") {
    val lines =
      """
        |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
        |""".stripMargin
    assert(Day2Part1.check(Source.fromString(lines).getLines().toSeq, max) == 8)
  }

  test("Compute real case") {
    val result = Day2Part1.check(Source.fromResource("p23/day2/input.txt").getLines().toSeq, max)
    assert(result == 2685)
  }
}
