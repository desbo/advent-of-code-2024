package aoc
package twentyfour

import aoc.twentyfour.Day9.blocks
import cats.syntax.all.*

object Day9 extends Solution[String, Int]:
  override def parse(input: String): String = input

  def blocks(input: String): String =
    input.zipWithIndex
      .flatMap:
        case (char, i) if i % 2 == 0 =>
          List.fill(char.toString.toInt)((i / 2).toString)
        case (char, i) =>
          List.fill(char.toString.toInt)(".")
      .mkString

  override def part1(input: String): Int =
    val b         = blocks(input)
    val freeSpace = b.count(_ == '.')

    def process(blocks: List[Char], remaining: List[Char]): String =
      blocks match
        case a :: b => ???
        case Nil    => ???

    process(b.toList, b.reverse.filterNot(_ == '.').take(freeSpace).toList)
