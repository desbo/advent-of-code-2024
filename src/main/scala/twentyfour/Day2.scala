package aoc
package twentyfour

object Day2 extends Solution[List[List[Int]], Int] {
  override def parse(input: String): List[List[Int]] =
    input.split('\n').map(_.split(' ').map(_.toInt).toList).toList

  def safe(xs: List[Int]): Boolean =
    val descending = xs.head > xs(1)

    xs
      .sliding(2)
      .forall:
        case List(l, r) =>
          (if (descending) l > r else l < r) && math.abs(l - r) < 3

  override def part1(input: List[List[Int]]): Int =
    input.count(safe)

  override def part2(input: List[Int]): Int = ???
}
