package p23.day8

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day8Part2Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |LR
        |
        |11A = (11B, XXX)
        |11B = (XXX, 11Z)
        |11Z = (11B, XXX)
        |22A = (22B, XXX)
        |22B = (22C, 22C)
        |22C = (22Z, 22Z)
        |22Z = (22B, 22B)
        |XXX = (XXX, XXX)
        |""".stripMargin.trim

    assert(Day8Part2.check(Source.fromString(lines).mkString) == 6)
  }

  test("Compute real case") {
    val result = Day8Part2.check(Source.fromResource("p23/day8/input.txt").mkString)
    assert(result == 13289612809129L)
  }
}
