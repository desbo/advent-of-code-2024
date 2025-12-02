package twentyfive

import aoc.twentyfive.Day2

class Day2Test extends munit.FunSuite {
  test("p2 example") {
    val input =
      "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

    assertEquals(Day2.part2(Day2.parse(input)), 4174379265L)
  }

}
