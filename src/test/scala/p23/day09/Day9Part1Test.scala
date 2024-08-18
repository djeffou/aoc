package p23.day09

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day9Part1Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |0 3 6 9 12 15
        |""".stripMargin.trim

    assert(Day9Part1.check(lines) == 18)
  }

  test("Check example 2") {
    val lines =
      """
        |0 3 6 9 12 15
        |1 3 6 10 15 21
        |10 13 16 21 30 45
        |""".stripMargin.trim

    assert(Day9Part1.check(lines) == 114)
  }

  test("Check example 3") {
    val lines =
      """
        |12 15 30 67 130 208 262 208 -104 -915 -2586 -5631 -10754 -18890 -31250 -49370 -75164 -110981 -159666 -224625 -309894
        |""".stripMargin.trim

    assert(Day9Part1.check(lines) == -420212)
  }

  test("Compute real case") {
    val result = Day9Part1.check(Source.fromResource("p23/day9/input.txt").mkString)
    assert(result == 1939607039)
  }
}
