package aoc
package twentyfour

object Day1 extends Solution[List[(Int, Int)], Long] {
  override def parse(input: String): List[(Int, Int)] =
    input
      .split('\n')
      .map { line =>
        line.split("   ").toList match
          case List(l, r) => (l.toInt, r.toInt)
          case other      => sys.error(other.mkString(""))
      }
      .toList

  override def part1(input: List[(Int, Int)]): Long =
    val lefts  = input.map(_._1)
    val rights = input.map(_._2)
    lefts.sorted
      .zip(rights.sorted)
      .map:
        case (l, r) => math.abs(l - r)
      .sum

  override def part2(input: List[(Int, Int)]): Long =
    val counts = input
      .map(_._2)
      .groupBy(identity)
      .view
      .mapValues(_.length)

    input
      .map(_._1)
      .foldLeft(0): (sum, loc) =>
        sum + (loc * counts.getOrElse(loc, 0))
}
