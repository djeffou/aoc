package p23.day10

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day10Part2Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |...........
        |.S-------7.
        |.|F-----7|.
        |.||.....||.
        |.||.....||.
        |.|L-7.F-J|.
        |.|..|.|..|.
        |.L--J.L--J.
        |...........
        |""".stripMargin.trim

    assert(Day10Part2.check(lines) == 4)
  }

  test("Check example 2") {
    val lines =
      """
        |..........
        |.S------7.
        |.|F----7|.
        |.||OOOO||.
        |.||OOOO||.
        |.|L-7F-J|.
        |.|II||II|.
        |.L--JL--J.
        |..........
        |""".stripMargin.trim

    assert(Day10Part2.check(lines) == 4)
  }

  test("Check example 3") {
    val lines =
      """
        |.F----7F7F7F7F-7....
        |.|F--7||||||||FJ....
        |.||.FJ||||||||L7....
        |FJL7L7LJLJ||LJ.L-7..
        |L--J.L7...LJS7F-7L7.
        |....F-J..F7FJ|L7L7L7
        |....L7.F7||L7|.L7L7|
        |.....|FJLJ|FJ|F7|.LJ
        |....FJL-7.||.||||...
        |....L---J.LJ.LJLJ...
        |""".stripMargin.trim

    assert(Day10Part2.check(lines) == 8)
  }

  test("Compute real case") {
    val result = Day10Part2.check(Source.fromResource("p23/day10/input.txt").mkString)
    assert(result == 451)
  }
}
