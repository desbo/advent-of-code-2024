package aoc.twentyfour

class Day7Test extends munit.FunSuite {
  val sample = """190: 10 19
                 |3267: 81 40 27
                 |83: 17 5
                 |156: 15 6
                 |7290: 6 8 6 15
                 |161011: 16 10 13
                 |192: 17 8 14
                 |21037: 9 7 18 13
                 |292: 11 6 16 20""".stripMargin

  test("part1") {
    assertEquals(Day7.part1(Day7.parse(sample)), BigInt(3749))
  }
  test("part2") {
    assertEquals(Day7.part2(Day7.parse(sample)), BigInt(11387))
  }

}
