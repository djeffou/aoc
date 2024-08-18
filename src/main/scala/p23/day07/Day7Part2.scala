package p23.day07

import p23.day07.Day7Part1.HandType

import scala.io.Source
import scala.util.Try

//noinspection DuplicatedCode
object Day7Part2 {

  final case class Bid(hand: Hand, value: Int)

  final case class Hand(cards: Seq[Card]) extends Ordered[Hand] {

    override def compare(other: Hand): Int =
      (Day7Part2.getHandType(cards), Day7Part2.getHandType(other.cards)) match
        case (a, b) if a.rank != b.rank => a.rank - b.rank
        case _ => cards.map(_.value).zip(other.cards.map(_.value))
          .dropWhile((c, d) => c == d).headOption.map((c, d) => c - d).getOrElse(0)
  }

  enum Card(val value: Int):
    case A extends Card(13)
    case K extends Card(12)
    case Q extends Card(11)
    case T extends Card(10)
    case n9 extends Card(9)
    case n8 extends Card(8)
    case n7 extends Card(7)
    case n6 extends Card(6)
    case n5 extends Card(5)
    case n4 extends Card(4)
    case n3 extends Card(3)
    case n2 extends Card(2)
    case J extends Card(1)

  private def parseCard(char: Char): Option[Card] =
    char match
      case i if i.isDigit => Card.values.find(_.value == i.asDigit)
      case other => Try(Card.valueOf(other.toString.toUpperCase)).toOption

  def parseLine(line: String): Option[Bid] =
    line match
      case s"$first $second" =>
        (first.flatMap(parseCard), second.toIntOption) match
          case (seq, Some(nb)) if seq.size == 5 => Some(Bid(Hand(seq), nb))
          case _ => None
      case _ => None

  def getHandType(cards: Seq[Card]): HandType = {
    val nbJ = cards.count(_ == Card.J)

    cards.filter(a => a != Card.J).groupBy(_.value).values.map(_.size).toList.sorted.reverse match
      case a :: tail if a + nbJ == 5 => HandType.FiveOfAKing
      case Nil if nbJ == 5 => HandType.FiveOfAKing

      case a :: tail if a + nbJ == 4 => HandType.FourOfAKing

      case 3 :: tail if nbJ == 2 => HandType.FullHouse
      case 3 :: 2 :: tail => HandType.FullHouse
      case 2 :: 2 :: tail if nbJ == 1 => HandType.FullHouse
      case 2 :: 1 :: tail if nbJ == 2 => HandType.FullHouse

      case a :: tail if a + nbJ == 3 => HandType.ThreeOfAKind

      case 2 :: 2 :: tail => HandType.TwoPair
      case 2 :: tail if nbJ == 1 => HandType.TwoPair

      case a :: tail if a + nbJ == 2 => HandType.OnePair

      case _ => HandType.HighCard
  }

  def check(all: String): Int =
    Source.fromString(all).getLines()
      .flatMap(parseLine)
      .toSeq.sortBy(_.hand)
      .zipWithIndex
      .map((bid, index) => bid.value * (index + 1))
      .sum
}

