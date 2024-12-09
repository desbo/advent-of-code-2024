package aoc.twentyfour

import aoc.twentyfour.Day9.*

class Day9Test extends munit.FunSuite {
  test("blocks") {
    assertEquals(Day9.render(Day9.parse("12345")), "0..111....22222")
    assertEquals(Day9.render(Day9.parse("2333133121414131402")), "00...111...2...333.44.5555.6666.777.888899")
  }

  test("defrag") {
    assertEquals(
      Day9.render(Day9.defrag(Day9.parse("12345"))),
      "022111222"
    )

    assertEquals(
      Day9.render(Day9.defrag(Day9.parse("2333133121414131402"))),
      "0099811188827773336446555566"
    )
  }

  test("defrag 2") {
//    assertEquals(
//      Day9.render(Day9.defrag2(Day9.parse("12345"))),
//      "00992111777.44.333....5555.6666.....8888.."
//    )

    assertEquals(
      Day9.render(Day9.defrag2(Day9.parse("2333133121414131402"))),
      "00992111777.44.333....5555.6666.....8888.."
    )
  }

  test("checksum") {
    val parsed    = Day9.parse("2333133121414131402")
    val defragged = Day9.defrag(parsed)
    assertEquals(Day9.checksum(defragged), BigInt(1928))
  }

  test("checksum 2") {
    val parsed    = Day9.parse("2333133121414131402")
    val defragged = Day9.defrag2(parsed)
    assertEquals(Day9.checksum(defragged), BigInt(2858))
  }

  test("checksum 2b") {
    val parsed         = Day9.parse("12345")
    val defragged      = Day9.defrag2(parsed)
    val defraggedagain = Day9.defrag2(defragged)
    println(render(defraggedagain))
    assertEquals(Day9.checksum(defraggedagain), BigInt(132))
  }

  test("take") {
    val blocks = List(0, 3, 2, 1).map(s => Block(s, 1, true))
    assertEquals(take(blocks, 1).map(_.size), List(0, 1))
    assertEquals(take(blocks, 2).map(_.size), List(0, 2))
    assertEquals(take(blocks, 3).map(_.size), List(0, 3))
    assertEquals(take(blocks, 4).map(_.size), List(0, 3, 1))
    assertEquals(take(blocks, 5).map(_.size), List(0, 3, 2))
    assertEquals(take(blocks, 6).map(_.size), List(0, 3, 2, 1))
    assertEquals(take(blocks, 7).map(_.size), List(0, 3, 2, 1))
  }

  test("drop") {
    val blocks = List(0, 3, 2, 1).map(s => Block(s, 1, true))
    assertEquals(drop(blocks, 1).map(_.size), List(2, 2, 1))
    assertEquals(drop(blocks, 2).map(_.size), List(1, 2, 1))
    assertEquals(drop(blocks, 3).map(_.size), List(2, 1))
    assertEquals(drop(blocks, 4).map(_.size), List(1, 1))
    assertEquals(drop(blocks, 5).map(_.size), List(1))
    assertEquals(drop(blocks, 6).map(_.size), List.empty)
    assertEquals(drop(blocks, 7).map(_.size), List.empty)
  }
}
