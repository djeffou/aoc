package p23.day09

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day9Part2Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |10 13 16 21 30 45
        |""".stripMargin.trim

    assert(Day9Part2.check(lines) == 5)
  }

  test("Check example 2") {
    val lines =
      """
        |0 3 6 9 12 15
        |1 3 6 10 15 21
        |10 13 16 21 30 45
        |""".stripMargin.trim

    assert(Day9Part2.check(lines) == 2)
  }

  test("Compute real case") {
    val result = Day9Part2.check(Source.fromResource("p23/day9/input.txt").mkString)
    assert(result == 1041)
  }
}
