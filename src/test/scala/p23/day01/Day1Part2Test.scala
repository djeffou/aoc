package p23.day01

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day1Part2Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |two1nine
        |eightwothree
        |abcone2threexyz
        |xtwone3four
        |4nineeightseven2
        |zoneight234
        |7pqrstsixteen
        |""".stripMargin
    assert(Day1Part2.getSum(Source.fromString(lines).getLines().toSeq) == 281)
  }

  test("Check example 2") {
    assert(Day1Part2.getSum(Seq("eightwo")) == 82)
    assert(Day1Part2.getSum(Seq("eightwo")) != 88)
  }

  test("Compute real case") {
    val result = Day1Part2.getSum(Source.fromResource("p23/day01/input.txt").getLines().toSeq)
    assert(result == 54094)
  }
}
