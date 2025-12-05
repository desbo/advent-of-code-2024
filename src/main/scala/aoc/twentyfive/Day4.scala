package aoc.twentyfive

import aoc.Solution
import aoc.util.{Direction, Grid, Vec2}
import cats.syntax.all.*

object Day4 extends Solution[Grid[Char], Int]:
  override def parse(input: String): Grid[Char] = Grid.chars(input)

  def countNeighbours(grid: Grid[Char], position: Vec2): Int =
    Direction.neighbours8(position).flatMap(grid.at).count(_ == '@')

  override def part1(input: Grid[Char]): Int =
    input.positionalFoldLeft(0):
      case (count, (pos, '@')) if countNeighbours(input, pos) < 4 => count + 1
      case (count, _)                                             => count

  override def part2(input: Grid[Char]): Int =
    val iterations = LazyList.iterate(input): grid =>
      grid.positionalMap:
        case ('@', pos) if countNeighbours(grid, pos) < 4 => '.'
        case (c, _)                                       => c

    iterations.map(part1).takeWhile(_ > 0).sum
