package aoc
package twentyfour

object Day3 extends Solution[String, Int]:
  override def parse(input: String): String = input

  override def part1(input: String): Int =
    """mul\((\d+),(\d+)\)""".r
      .findAllMatchIn(input)
      .foldLeft(0):
        case (sum, m) =>
          sum + (m.group(1).toInt * m.group(2).toInt)

  override def part2(input: String): Int =
    input
      .splitWithDelimiters("""do\(\)|don't\(\)""", 0)
      .prepended("do()")
      .grouped(2)
      .foldLeft(0):
        case (sum, Array("do()", mul)) => sum + part1(mul)
        case (sum, _)                  => sum
