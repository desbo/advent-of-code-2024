package aoc.twentyfive

import aoc.Solution

import scala.annotation.tailrec

object Day6 extends Solution[String, Long]:
  case class Problem(numbers: List[Long], op: Char):
    def solve: Long = op match
      case '+' => numbers.sum
      case '*' => numbers.product

  override def parse(input: String): String =
    input

  override def part1(input: String): Long =
    val transposed = input.linesIterator.toList
      .map(_.trim.split("\\s+").toList)
      .transpose

    transposed
      .map: t =>
        Problem(t.dropRight(1).map(_.toLong), t.last.head).solve
      .sum

  def rtl(input: String): List[Problem] =
    val charMatrix = input.linesIterator.toList.map(_.toCharArray.toList)

    @tailrec
    def scan(
        x: Int,
        y: Int,
        currentNum: String,
        nums: List[String],
        op: Option[Char],
        problems: List[Problem]
    ): List[Problem] =
      if x < 0 then problems
      else
        charMatrix(y)(x) match
          case d if d.isDigit => // add to current digit and continue down
            scan(x, y + 1, currentNum.appended(d), nums, op, problems)
          case ' ' if y == charMatrix.size - 1 => // start a new digit at the next column
            scan(x - 1, 0, "", nums.appended(currentNum), op, problems)
          case ' ' => // empty cell, continue
            scan(x, y + 1, currentNum, nums, op, problems)
          case op => // op signals the end of the block, build the problem
            val p = Problem(nums.appended(currentNum).map(_.toLong), op)
            scan(x - 2, 0, "", List.empty, None, problems.appended(p))

    scan(charMatrix.head.size - 1, 0, "", List.empty, None, List.empty)

  override def part2(input: String): Long =
    rtl(input).map(_.solve).sum
