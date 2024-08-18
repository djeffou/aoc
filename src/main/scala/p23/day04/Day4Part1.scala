package p23.day04

object Day4Part1 {

  private final case class Card(cardNb: Long, winningNbs: Seq[Long], numbers: Seq[Long])

  private def parseLine(line: String): Option[Card] = line match
    case s"Card$cardNb:$w|$n" => cardNb.trim.toIntOption.map(i =>
      Card(i, w.split(' ').toSeq.flatMap(_.trim.toLongOption), n.split(' ').toSeq.flatMap(_.trim.toLongOption))
    )
    case _ => None

  private def computeLinePoints(card: Card): Long = {
    val r: Seq[Long] = card.numbers.filter(n => card.winningNbs.contains(n)).toList
    r match
      case head :: Nil => 1L
      case head :: tail => r.tail.scan(1L)((a, _) => a * 2L).last
      case _ => 0L
  }

  def check(lines: Seq[String]): Long =
    lines.flatMap(parseLine).map(computeLinePoints).sum
}
