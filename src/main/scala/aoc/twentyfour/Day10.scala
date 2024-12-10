package aoc
package twentyfour

import cats.syntax.all.*

import util.*

object Day10 extends Solution[Grid[Int], Int]:
  override def parse(input: String): Grid[Int] =
    Grid.chars(input).map(_.toString.toInt)

  override def part1(map: Grid[Int]): Int =
    def ninesReachableFrom(pos: Vec2, nines: Set[Vec2]): Set[Vec2] =
      val height = map.points(pos)

      if height == 9 then nines + pos
      else
        Direction
          .neighbours(pos)
          .filter(n => map.points.get(n).contains(height + 1))
          .foldLeft(Set.empty[Vec2]):
            case (set, p) =>
              set ++ ninesReachableFrom(p, Set.empty)

    map.points
      .map:
        case (point, 0) => ninesReachableFrom(point, Set.empty).size
        case _          => 0
      .sum

  override def part2(map: Grid[Int]): Int =
    def routesToNines(pos: Vec2, route: List[Vec2], routes: Set[List[Vec2]]): Set[List[Vec2]] =
      val height = map.points(pos)

      if height == 9 then routes + (route :+ pos)
      else
        Direction
          .neighbours(pos)
          .filter(n => map.points.get(n).contains(height + 1))
          .foldLeft(routes):
            case (r, p) =>
              r ++ routesToNines(p, route :+ pos, r)

    map.points
      .map:
        case (point, 0) => routesToNines(point, List.empty, Set.empty).size
        case _          => 0
      .sum
