package aoc.twentyfive

import aoc.Solution

import scala.annotation.tailrec

object Day3 extends Solution[List[List[Int]], Long]:
  override def parse(input: String): List[List[Int]] =
    input.linesIterator.map(_.map(_.asDigit).toList).toList

  def maxJolts(length: Int)(bank: List[Int]): Long =
    @tailrec
    def go(section: List[Int], remaining: Int, jolts: Long): Long =
      val (a, i) = section.dropRight(remaining).zipWithIndex.maxBy(_._1)
      val rest   = section.drop(i + 1)

      val newJolts = jolts + a * math.pow(10, remaining).toLong
      if remaining == 1 then newJolts + rest.max
      else go(rest, remaining - 1, newJolts)

    go(bank, length - 1, 0)

  override def part1(input: List[List[Int]]): Long =
    input.map(maxJolts(2)).sum

  override def part2(input: List[List[Int]]): Long =
    input.map(maxJolts(12)).sum
