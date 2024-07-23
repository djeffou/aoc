package p23.day10

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day10Part1Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |.....
        |.S-7.
        |.|.|.
        |.L-J.
        |.....
        |""".stripMargin.trim

    assert(Day10Part1.check(lines) == 4)
  }

  test("Check example 2") {
    val lines =
      """
        |-L|F7
        |7S-7|
        |L|7||
        |-L-J|
        |L|-JF
        |""".stripMargin.trim

    assert(Day10Part1.check(lines) == 4)
  }

  test("Check example 3") {
    val lines =
      """
        |..F7.
        |.FJ|.
        |SJ.L7
        ||F--J
        |LJ...
        |""".stripMargin.trim

    assert(Day10Part1.check(lines) == 8)
  }

  test("Compute real case") {
    val result = Day10Part1.check(Source.fromResource("p23/day10/input.txt").mkString)
    assert(result == 6838)
  }
}
