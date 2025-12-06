package twentyfive

import aoc.twentyfive.Day6
import aoc.twentyfive.Day6.Problem

class Day6Test extends munit.FunSuite:
  val sample = """123 328  51 64 
                 | 45 64  387 23 
                 |  6 98  215 314
                 |*   +   *   +  """.stripMargin

  test("p1") {
    assertEquals(Day6.part1(Day6.parse(sample)), 4277556L)
  }

  test("rtl parsing") {
    val expected = List(
      Problem(List(4, 431, 623), '+'),
      Problem(List(175, 581, 32), '*'),
      Problem(List(8, 248, 369), '+'),
      Problem(List(356, 24, 1), '*')
    )
    assertEquals(Day6.rtl(sample), expected)
  }
