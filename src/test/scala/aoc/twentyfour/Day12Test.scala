package aoc.twentyfour

class Day12Test extends munit.FunSuite {

  test("example") {
    val parsed = Day12.parse("AAAA\nBBCD\nBBCC\nEEEC")
    println(Day12.regions(parsed))

  }

}
