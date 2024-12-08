package aoc.twentyfour

class Day8Test extends munit.FunSuite {
  val map = Day8.parse("""............
      |........0...
      |.....0......
      |.......0....
      |....0.......
      |......A.....
      |............
      |............
      |........A...
      |.........A..
      |............
      |............""".stripMargin)

  test("sample part1") {
    assertEquals(Day8.part1(map), 14)
  }

  test("sample part2") {
    assertEquals(Day8.part2(map), 34)
  }

}
