package p23.day02

object Day2Part1 {

  final case class Game(id: Long, tuples: Seq[(Int, String)])

  private def parseSets(s: String): Seq[String] = s.split(";")

  private def parseSet(s: String): Seq[String] = s.split(",").map(_.trim)

  private def parseNbColorTuple(s: String): Option[(Int, String)] = {
    s.trim.split(" ").toSeq match
      case head +: _ :+ last => head.toIntOption.map(i => (i, last))
      case _ => None
  }

  def parseLine(line: String): Option[Game] = {
    val idOpt = line.split(":").headOption.flatMap(_.split(" ").lastOption).flatMap(_.toLongOption)
    val tuples = line.split(":").lastOption.toSeq.flatMap(parseSets).flatMap(parseSet).flatMap(parseNbColorTuple)

    idOpt.map(id => Game(id, tuples))
  }

  def check(lines: Seq[String], maxValues: Map[String, Int]): Long = {
    lines.flatMap(parseLine).filter(game => {
      game.tuples.forall((nb, string) => {
        maxValues.get(string) match
          case Some(value) => value >= nb
          case None => true
      })
    }).map(_.id).sum
  }

}
