package p23.day7

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day7Part2Test extends AnyFunSuite {

  test("Check example 1") {
    assert(Day7Part2.getHandType(Day7Part2.parseLine("32T3K 765").get.hand.cards) == Day7Part1.HandType.OnePair)
    assert(Day7Part2.getHandType(Day7Part2.parseLine("T55J5 684").get.hand.cards) == Day7Part1.HandType.FourOfAKing)
    assert(Day7Part2.getHandType(Day7Part2.parseLine("KK677 28").get.hand.cards) == Day7Part1.HandType.TwoPair)
    assert(Day7Part2.getHandType(Day7Part2.parseLine("KTJJT 220").get.hand.cards) == Day7Part1.HandType.FourOfAKing)
    assert(Day7Part2.getHandType(Day7Part2.parseLine("QQQJA 483").get.hand.cards) == Day7Part1.HandType.FourOfAKing)
  }

  test("Check example 3") {
    val lines =
      """
        |32T3K 765
        |T55J5 684
        |KK677 28
        |KTJJT 220
        |QQQJA 483
        |""".stripMargin.trim

    assert(Day7Part2.check(Source.fromString(lines).mkString) == 5905)
  }

  test("Compute real case") {
    val result = Day7Part2.check(Source.fromResource("p23/day7/input.txt").mkString)
    assert(result == 249631254)
  }
}
