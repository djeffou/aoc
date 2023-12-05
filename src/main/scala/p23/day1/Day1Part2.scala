package p23.day1

object Day1Part2 {

  private val stringToInt = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )
  private val regexWordsAndNumber = ("(?=(" + stringToInt.keys.mkString("|") + "|[0-9]))").r

  private def getLineSumIfAny(line: String): Long = {
    val groups =  regexWordsAndNumber.findAllMatchIn(line).toSeq.map(_.group(1))
    groups.map(line => stringToInt.getOrElse(line, line)) match
      case head :: Nil => (head + head).toLong
      case head :: tail => (head + tail.last).toLong
      case _ => 0
  }

  def getSum(lines: Seq[String]): Long =
    lines.map(getLineSumIfAny).sum
}
