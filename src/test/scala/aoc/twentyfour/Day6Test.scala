package aoc
package twentyfour

class Day6Test extends munit.FunSuite {
  val map =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin

  test("part1") {
    assertEquals(Day6.part1(Day6.parse(map)), 41)
  }
  test("part2") {
    assertEquals(Day6.part2(Day6.parse(map)), 6)
  }
}
