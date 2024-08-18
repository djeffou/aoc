package p23.day01

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day1Part1Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |1abc2
        |pqr3stu8vwx
        |a1b2c3d4e5f
        |treb7uchet
        |""".stripMargin
    assert(Day1Part1.getSum(Source.fromString(lines).getLines().toSeq) == 142)
  }

  test("Compute real case") {
    val result = Day1Part1.getSum(Source.fromResource("p23/day01/input.txt").getLines().toSeq)
    assert(result == 54968)
  }
}
