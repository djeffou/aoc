package p23.day12

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day12Part1Test extends AnyFunSuite {

  test("Example 1") {
    assert(Day12Part1.check("???.### 1,1,3") == 1)
    assert(Day12Part1.check(".??..??...?##. 1,1,3") == 4)
    assert(Day12Part1.check("?#?#?#?#?#?#?#? 1,3,1,6") == 1)
    assert(Day12Part1.check("????.#...#... 4,1,1") == 1)
    assert(Day12Part1.check("????.######..#####. 1,6,5") == 4)
    assert(Day12Part1.check("?###???????? 3,2,1") == 10)
  }

  test("Compute real case") {
    val result = Day12Part1.check(Source.fromResource("p23/day12/input.txt").mkString)
    assert(result == 7843)
  }
}
