package utils

import scala.io.Source

object Utils {

  def getLines(lines: String): Seq[String] =
    Source.fromString(lines.trim).getLines().toSeq

  def getLinesFromSource(path: String): Seq[String] =
    Source.fromResource(path).getLines().toSeq

}
