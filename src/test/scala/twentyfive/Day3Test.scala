package twentyfive

import aoc.twentyfive.Day3

class Day3Test extends munit.FunSuite {
  val input = Day3.parse("""987654321111111
                |811111111111119
                |234234234234278
                |818181911112111""".stripMargin)

  test("p1") {
    assertEquals(Day3.part1(input), 357L)
  }

  test("p2") {
    assertEquals(Day3.part2(input), 3121910778619L)
  }
}
