package p23.day4

import scala.annotation.tailrec

object Day4Part2 {

  private final case class Card(cardNb: Int, winningNbs: Seq[Int], numbers: Seq[Int])

  private def parseLine(line: String): Option[Card] = line match
    case s"Card$cardNb:$w|$n" => cardNb.trim.toIntOption.map(i =>
      Card(i, w.split(' ').toSeq.flatMap(_.trim.toIntOption), n.split(' ').toSeq.flatMap(_.trim.toIntOption))
    )
    case _ => None

  private def computeLinePoints(card: Card): Int =
    card.numbers.count(n => card.winningNbs.contains(n))

  private def getScratchcardsCount(cards: Seq[Card]): Int = {
    @tailrec
    def inner(remainingCards: Seq[Card], nextMultipliers: Seq[Int], acc: Int): Int = {
      remainingCards match
        case head :: tail =>
          val points = computeLinePoints(head)
          val multiplier = nextMultipliers.head
          val (firstPart, lastPart) = nextMultipliers.drop(1).splitAt(points)
          val newMultipliers = firstPart.map(_ + multiplier) ++ lastPart
          inner(tail, newMultipliers, multiplier + acc)
        case _ => acc
    }

    inner(cards, Seq.fill(cards.size)(1), 0)
  }

  def check(lines: Seq[String]): Int =
    getScratchcardsCount(lines.flatMap(parseLine))
}
