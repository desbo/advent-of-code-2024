package aoc.twentyfour

import aoc.twentyfour.Day9.*

class Day9Test extends munit.FunSuite {
  test("blocks") {
    assertEquals(Day9.render(Day9.parse("12345")), "0..111....22222")
    assertEquals(Day9.render(Day9.parse("2333133121414131402")), "00...111...2...333.44.5555.6666.777.888899")
  }

  test("defrag") {
//    assertEquals(Day9.render(Day9.defrag(Day9.parse("12345"))), "022111222")
    assertEquals(
      Day9.render(Day9.defrag(Day9.parse("2333133121414131402"))),
      "0099811188827773336446555566"
    )
  }

  test("checksum") {
    val parsed    = Day9.parse("2333133121414131402")
    val defragged = Day9.defrag(parsed)
    assertEquals(Day9.checksum(defragged), BigInt(1928))
  }

  test("take") {
    val blocks = List(Block(10, 1, false), Block(20, 1, false), Block(10, 1, false))
    assertEquals(take(blocks, 11), List(Block(10, 1, false), Block(1, 1, false)))
    assertEquals(take(blocks, 31), List(Block(10, 1, false), Block(20, 1, false), Block(1, 1, false)))
    assertEquals(take(blocks, 0), List.empty)
    assertEquals(take(blocks, 99), blocks)

    assertEquals(
      take(
        List(
          Block(2, 9, false),
          Block(0, -999, true),
          Block(4, 8, false),
          Block(1, -999, true),
          Block(3, 7, false),
          Block(1, -999, true),
          Block(3, 6, false),
          Block(2, 6, false),
          Block(1, 6, false)
        ),
        14
      ).filterNot(_.empty),
      List(
        Block(2, 9, false),
        Block(4, 8, false),
        Block(3, 7, false),
        Block(3, 6, false)
      )
    )
  }

  test("drop") {
    val blocks = List(Block(10, 1, false), Block(20, 1, false))
    assertEquals(drop(blocks, 11), List(Block(19, 1, false)))
    assertEquals(drop(blocks, 0), blocks)
    assertEquals(drop(blocks, 99), List.empty)
  }

  test("pack") {
    val blocks = List(Block(1, 1, false), Block(1, 1, false), Block(1, 2, false), Block(1, 1, false))
    assertEquals(pack(blocks), List(Block(2, 1, false), Block(1, 2, false), Block(1, 1, false)))
  }
}
