package p23.day6

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day6Part2Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |Time:      7  15   30
        |Distance:  9  40  200
        |""".stripMargin.trim

    assert(Day6Part2.check(Source.fromString(lines).mkString) == 71503)
  }

  test("Compute real case") {
    val result = Day6Part2.check(Source.fromResource("p23/day6/input.txt").mkString)
    assert(result == 21039729)
  }
}
