package aoc.twentyfour

class Day9Test extends munit.FunSuite {
  test("blocks") {
    assertEquals(Day9.blocks("12345"), "0..111....22222")
    assertEquals(Day9.blocks("2333133121414131402"), "00...111...2...333.44.5555.6666.777.888899")
  }
}
