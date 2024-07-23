package p23.day11

import org.scalatest.funsuite.{AnyFunSuite, AnyFunSuiteLike}
import p23.day11.Day11Part1.Point

import scala.io.Source

class Day11Part2Test extends AnyFunSuite {

  test("Check example 1") {
    val lines =
      """
        |...#......
        |.......#..
        |#.........
        |..........
        |......#...
        |.#........
        |.........#
        |..........
        |.......#..
        |#...#.....
        |""".stripMargin.trim

    val result = Day11Part2.check(lines, 2)
    assert(result == 374)
  }

  test("Check example 2") {
    val lines =
      """
        |...#......
        |.......#..
        |#.........
        |..........
        |......#...
        |.#........
        |.........#
        |..........
        |.......#..
        |#...#.....
        |""".stripMargin.trim

    val result = Day11Part2.check(lines, 10)
    assert(result == 1030)
  }

  test("Check example 3") {
    val lines =
      """
        |...#......
        |.......#..
        |#.........
        |..........
        |......#...
        |.#........
        |.........#
        |..........
        |.......#..
        |#...#.....
        |""".stripMargin.trim

    val result = Day11Part2.check(lines, 100)
    assert(result == 8410)
  }

  test("Compute real case") {
    val result = Day11Part2.check(Source.fromResource("p23/day11/input.txt").mkString, 1000000)
    assert(result == 752936133304L)
  }
}
