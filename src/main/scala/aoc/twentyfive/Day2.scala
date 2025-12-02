package aoc.twentyfive

import aoc.Solution
import Day2.*

import scala.annotation.tailrec

object Day2 extends Solution[List[IdRange], Long]:
  case class IdRange(start: String, end: String):
    def containsEvenLengths: Boolean =
      (start.length to end.length).exists(_ % 2 == 0)

    def expand: Seq[String] =
      (start.toLong to end.toLong).map(_.toString)

  override def parse(input: String): List[IdRange] =
    input.trim
      .split(",")
      .toList
      .map: s =>
        val spl = s.split("-")
        IdRange(spl(0), spl(1))

  override def part1(input: List[IdRange]): Long =
    def valid(id: String): Boolean =
      val (l, r) = id.splitAt(id.length / 2)
      l != r

    input
      .filter(_.containsEvenLengths)
      .flatMap(_.expand)
      .filterNot(valid)
      .map(_.toLong)
      .sum

  override def part2(input: List[IdRange]): Long =
    def invalid(id: String): Boolean =
      @tailrec
      def go(groups: Int): Boolean = {
        val groupSize = id.length / groups
        if groups > id.length then false
        else if id.grouped(groupSize).toSet.size == 1 then true
        else go(groups + 1)
      }

      id.length > 1 && go(2)

    input
      .flatMap(_.expand)
      .filter(invalid)
      .map(_.toLong)
      .sum
