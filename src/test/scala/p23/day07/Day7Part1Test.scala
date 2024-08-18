package p23.day07

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day7Part1Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |32T3K 765
        |T55J5 684
        |KK677 28
        |KTJJT 220
        |QQQJA 483
        |""".stripMargin.trim

    assert(Day7Part1.check(Source.fromString(lines).mkString) == 6440)
  }

  test("Compute real case") {
    val result = Day7Part1.check(Source.fromResource("p23/day07/input.txt").mkString)
    assert(result == 248559379)
  }
}
