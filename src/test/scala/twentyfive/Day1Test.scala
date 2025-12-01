package twentyfive

import aoc.twentyfive.Day1

class Day1Test extends munit.FunSuite {
  test("part 2") {
    val in = Day1.parse("""L68
               |L30
               |R48
               |L5
               |R60
               |L55
               |L1
               |L99
               |R14
               |L82""".stripMargin)

    assertEquals(Day1.part2(in), 6)
  }

}
