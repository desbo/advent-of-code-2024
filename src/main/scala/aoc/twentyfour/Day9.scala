package aoc
package twentyfour

import aoc.twentyfour.Day9.blocks
import cats.syntax.all.*

import scala.annotation.tailrec

object Day9 extends Solution[String, Int]:
  override def parse(input: String): String = input

  def blocks(input: String): String =
    input.zipWithIndex
      .flatMap:
        case (char, i) if i % 2 == 0 =>
          List.fill(char.toString.toInt)((i / 2).toString)
        case (char, i) =>
          List.fill(char.toString.toInt)(".")
      .mkString

  def defrag(blocks: String): String =
    @tailrec
    def process(defragged: String, toProcess: List[Char], blocks: List[Char]): String =
      println(toProcess)
      toProcess match
        case b :: remaining =>
          blocks match
            case '.' :: rest => process(s"$defragged$b", remaining, rest)
            case c :: rest   => process(s"$defragged$c", b +: remaining, rest)
        case Nil => defragged

    val freeSpace = blocks.count(_ == '.')

    process("", blocks.reverse, blocks.toList)
      .padTo(blocks.length, '.')

//  def checksum(blocks: String): BigInt =

  override def part1(input: String): Int =
    defrag(blocks(input))
    1
