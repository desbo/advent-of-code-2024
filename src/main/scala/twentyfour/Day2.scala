package aoc
package twentyfour

import scala.jdk.CollectionConverters._

object Day2 extends Solution[List[List[Int]], Int] {
  override def parse(input: String): List[List[Int]] =
    input.lines().toList.asScala.map(_.split(' ').map(_.toInt).toList).toList

  def safe(xs: List[Int]): Boolean =
    val descending = xs.head > xs(1)

    xs
      .sliding(2)
      .forall:
        case List(l, r) =>
          (if (descending) l > r else l < r) && math.abs(l - r) <= 3 && math.abs(l - r) >= 1

  def safeWithOneDelete(xs: List[Int]): Boolean =
    xs.zipWithIndex
      .map: (_, idx) =>
        xs.patch(idx, Nil, 1)
      .exists(safe)

  override def part1(input: List[List[Int]]): Int =
    input.count(safe)

  override def part2(input: List[List[Int]]): Int =
    input.count: xs =>
      safe(xs) || safeWithOneDelete(xs)
}
