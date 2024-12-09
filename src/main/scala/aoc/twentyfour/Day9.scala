package aoc
package twentyfour

import aoc.twentyfour.Day9.blocks
import cats.syntax.all.*

import scala.annotation.tailrec

object Day9 extends Solution[String, BigInt]:
  override def parse(input: String): String = input

  def blocks(input: String): String =
    input
      .dropRight(1)
      .zipWithIndex
      .flatMap:
        case (char, i) if i % 2 == 0 =>
          List.fill(char.toString.toInt)((i / 2).toString)
        case (char, i) =>
          List.fill(char.toString.toInt)(".")
      .mkString

  def defrag(blocks: String): String =
    @tailrec
    def process(defragged: String, toProcess: List[Char], blocks: List[Char]): String =
      toProcess match
        case b :: remaining =>
          blocks match
            case '.' :: rest => process(s"$defragged$b", remaining, rest)
            case c :: rest   => process(s"$defragged$c", b +: remaining, rest)
        case Nil =>
          blocks match
            case '.' :: _ => defragged
            case c :: _   => defragged.concat(c.toString)

    val freeSpace = blocks.count(_ == '.')

    process("", blocks.reverse.take(freeSpace).filterNot(_ == '.').toList, blocks.toList)
      .padTo(blocks.length, '.')

  def checksum(blocks: String): BigInt =
    blocks
      .takeWhile(_ != '.')
      .zipWithIndex
      .foldLeft(BigInt(0)):
        case (sum, (char, idx)) =>
          sum + (char.toString.toInt * idx)

  override def part1(input: String): BigInt =
    println(blocks(input))
    println(defrag(blocks(input)))
    checksum(defrag(blocks(input)))
