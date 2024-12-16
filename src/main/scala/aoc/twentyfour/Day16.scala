package aoc
package twentyfour

import aoc.util.*

object Day16 extends Solution[Grid[Char], Long]:
  override def parse(input: String): Grid[Char] =
    Grid.chars(input)

  override def part1(input: Grid[Char]): Long =
    def search(maze: Grid[Char], d: Direction, steps: Long): Long =
      val pos = maze.positions('S').head
      maze.points
        .get(pos.offset(d))
        .fold(Long.MaxValue):
          case '#' =>
            math.min(
              search(maze, Direction.turn(d, Direction.Left), steps + 1000),
              search(maze, Direction.turn(d, Direction.Right), steps + 1000)
            )
          case 'E' => steps
          case '.' =>
            search(maze.swap(pos.offset(d), pos), d, steps + 1)

    search(input, Direction.Right, 0)
