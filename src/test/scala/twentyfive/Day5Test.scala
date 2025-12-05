package twentyfive

import aoc.twentyfive.Day5
import Day5.*

class Day5Test extends munit.FunSuite:
  test("compress") {
    val ranges = List(
      Range(3L, 5),
      Range(10L, 14),
      Range(16L, 20),
      Range(12L, 18)
    )

    val expected = List(
      Range(3, 5),
      Range(10, 20)
    )

    assertEquals(compress(ranges), expected)
  }

  test("example") {
    val input = """3-5
                  |10-14
                  |16-20
                  |12-18
                  |
                  |1
                  |5
                  |8
                  |11
                  |17
                  |32""".stripMargin

    assertEquals(Day5.part2(Day5.parse(input)), 14L)
  }
