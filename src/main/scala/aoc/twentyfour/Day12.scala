package aoc
package twentyfour

import aoc.util.*
import cats.syntax.all.*

import scala.annotation.tailrec

object Day12 extends Solution[Grid[Char], Long]:
  override def parse(input: String): Grid[Char] =
    Grid.chars(input)

  def fences(farm: Grid[Char])(point: Vec2): Set[Vec2] =
    val id = farm.points(point)

    Direction
      .neighbours(point)
      .filter: n =>
        !farm.covers(n) || !farm.points.get(n).contains(id)
      .toSet

  case class Region(id: Char, points: Set[Vec2]):
    val area: Int = points.size

    def perimeter(farm: Grid[Char]): Int =
      points.flatMap(fences(farm)).size

    def cost(farm: Grid[Char]): Long =
      area * perimeter(farm)

    def extend(point: Vec2): Region =
      copy(points = points + point)

  def regions(farm: Grid[Char]): Set[Region] =
    def discover(id: Char, point: Vec2, region: Region): Region =
      Direction
        .neighbours(point)
        .filter(p => farm.covers(p) && farm.points.get(p).contains(id))
        .foldLeft(region):
          case (region, n) => region.extend(n)

    def visited(regions: Set[Region], point: Vec2): Boolean =
      regions.flatMap(_.points)(point)

    farm.points.foldLeft(Set.empty[Region]):
      case (regions, (point, id)) if !visited(regions, point) =>
        regions + discover(id, point, Region(id, Set(point)))
      case (regions, _) => regions

  override def part1(input: Grid[Char]): Long =
    regions(input).map(_.cost(input)).sum
