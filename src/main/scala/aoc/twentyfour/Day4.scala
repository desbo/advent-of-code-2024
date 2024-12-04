package aoc
package twentyfour

import cats.syntax.all.*

object Day4 extends Solution[List[String], Int]:
  override def parse(input: String): List[String] = input.linesIterator.toList

  def count(input: List[String], offsets: List[List[(Int, Int)]], startX: Int, startY: Int, target: String): Int =
    offsets.foldLeft(0):
      case (c, offsets) =>
        val chars = offsets.foldLeft(List(input(startY)(startX))):
          case (chars, (y, x)) =>
            Either.catchNonFatal(input(startY + y)(startX + x)).fold(_ => chars, chars :+ _)

        if (chars.mkString == target) c + 1 else c

  override def part1(input: List[String]): Int =
    val LL = List((0, -1), (0, -2), (0, -3))
    val RR = List((0, 1), (0, 2), (0, 3))
    val UU = List((-1, 0), (-2, 0), (-3, 0))
    val DD = List((1, 0), (2, 0), (3, 0))
    val UL = List((-1, -1), (-2, -2), (-3, -3))
    val UR = List((-1, 1), (-2, 2), (-3, 3))
    val DL = List((1, -1), (2, -2), (3, -3))
    val DR = List((1, 1), (2, 2), (3, 3))

    val allDirections = List(LL, RR, UU, DD, UL, UR, DL, DR)

    input.zipWithIndex.foldLeft(0):
      case (totalCount, (line, y)) =>
        totalCount + line.zipWithIndex.foldLeft(0):
          case (c, (char, x)) =>
            if char == 'X' then c + count(input, allDirections, x, y, "XMAS")
            else c
