package p23.day02

import p23.day02.Day2Part1.Game

object Day2Part2 {

  private def reduceGameToMinimum(game: Game): Game = {
    val reducedTuples = game.tuples.groupMapReduce((nb, color) => color)((nb, color) => nb)(Math.max)
      .toSeq.map(_.swap)
    game.copy(tuples = reducedTuples)
  }

  def check(lines: Seq[String]): Int = {
    lines.flatMap(Day2Part1.parseLine).map(reduceGameToMinimum)
      .map(e => e.tuples.map(_._1).product).sum
  }

}
