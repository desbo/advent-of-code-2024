package aoc
package twentyfour

import util.{Grid, Vec2}

import cats.syntax.all.*

object Day8 extends Solution[Grid[Char], Int]:
  override def parse(input: String): Grid[Char] =
    Grid.chars(input)

  def antennaePositions(map: Grid[Char]): Map[Char, Set[Vec2]] =
    map.points
      .groupBy(_._2)
      .view
      .filterKeys(_ != '.')
      .mapValues(_.keySet)
      .toMap

  def antinodes(a: Vec2, b: Vec2): List[Vec2] =
    val diff = a - b
    List(a + diff, b - diff)

  def repeatingAntinodes(map: Grid[Char])(a: Vec2, b: Vec2): List[Vec2] =
    val diff = a - b
    LazyList.iterate(a)(_ + diff).takeWhile(map.covers).toList ++
      LazyList.iterate(b)(_ - diff).takeWhile(map.covers).toList

  def allAntinodes(map: Grid[Char], find: (Vec2, Vec2) => List[Vec2]): Iterable[Vec2] =
    antennaePositions(map).view.values
      .flatMap: positions =>
        positions.toList
          .combinations(2)
          .flatMap:
            case List(a, b) => find(a, b).filter(map.covers)

  override def part1(input: Grid[Char]): Int =
    allAntinodes(input, antinodes).toSet.size

  override def part2(input: Grid[Char]): Int =
    allAntinodes(input, repeatingAntinodes(input)).toSet.size
