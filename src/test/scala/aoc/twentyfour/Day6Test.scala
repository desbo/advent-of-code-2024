package aoc
package twentyfour

class Day6Test extends munit.FunSuite {
  test("sample") {
    val map = """....#.....
                |.........#
                |..........
                |..#.......
                |.......#..
                |..........
                |.#..^.....
                |........#.
                |#.........
                |......#...""".stripMargin

    assertEquals(Day6.part1(Day6.parse(map)), 41)
  }
}