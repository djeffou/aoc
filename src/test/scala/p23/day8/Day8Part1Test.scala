package p23.day8

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day8Part1Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |RL
        |
        |AAA = (BBB, CCC)
        |BBB = (DDD, EEE)
        |CCC = (ZZZ, GGG)
        |DDD = (DDD, DDD)
        |EEE = (EEE, EEE)
        |GGG = (GGG, GGG)
        |ZZZ = (ZZZ, ZZZ)
        |""".stripMargin.trim

    assert(Day8Part1.check(Source.fromString(lines).mkString) == 2)
  }

  test("Check example 2") {
    val lines =
      """
        |LLR
        |
        |AAA = (BBB, BBB)
        |BBB = (AAA, ZZZ)
        |ZZZ = (ZZZ, ZZZ)
        |""".stripMargin.trim

    assert(Day8Part1.check(Source.fromString(lines).mkString) == 6)
  }

  test("Compute real case") {
    val result = Day8Part1.check(Source.fromResource("p23/day8/input.txt").mkString)
    assert(result == 20777)
  }
}
