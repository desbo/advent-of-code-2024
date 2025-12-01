package aoc.twentyfive

import aoc.Solution

object Day1 extends Solution[List[String], Int]:
  override def parse(input: String): List[String] =
    input.linesIterator.toList

  override def part1(input: List[String]): Int =
    input
      .foldLeft((50, 0)):
        case ((pos, zeros), cmd) =>
          val dist = cmd.drop(1).toInt
          val next = cmd(0) match {
            case 'L' => (pos - dist) % 100
            case 'R' => (pos + dist) % 100
          }

          if next == 0 then (next, zeros + 1)
          else (next, zeros)
      ._2

  override def part2(input: List[String]): Int =
    input
      .foldLeft((50, 0)):
        case ((pos, zeros), cmd) =>
          val dist = cmd.drop(1).toInt

          cmd(0) match {
            case 'L' =>
              val hits = (pos until (pos - dist, -1)).map: x =>
                if x < 0 then (x + 100) % 100
                else x                  % 100

              (hits.last, zeros + hits.count(_ == 0))
            case 'R' =>
              val hits = (pos until (pos + dist)).map(_ % 100)

              (hits.last, zeros + hits.count(_ == 0))
          }
      ._2
