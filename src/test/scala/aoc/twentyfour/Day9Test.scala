package aoc.twentyfour

class Day9Test extends munit.FunSuite {
  test("blocks") {
    assertEquals(Day9.blocks("12345"), "0..111....22222")
    assertEquals(Day9.blocks("2333133121414131402"), "00...111...2...333.44.5555.6666.777.888899")
  }

  test("defrag") {
    assertEquals(Day9.defrag("0..111....22222"), "022111222......")
    assertEquals(
      Day9.defrag("00...111...2...333.44.5555.6666.777.888899"),
      "0099811188827773336446555566.............."
    )
  }

  test("checksum") {
    assertEquals(Day9.checksum("0099811188827773336446555566.............."), BigInt(1928))
  }
}
