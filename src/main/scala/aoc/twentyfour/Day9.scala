package aoc
package twentyfour

import aoc.twentyfour.Day9.Block
import cats.syntax.all.*

import scala.annotation.tailrec

object Day9 extends Solution[List[Block], BigInt]:
  case class Block(size: Int, id: Int, empty: Boolean):
    def remove(space: Int): Option[Block] =
      if space >= size then none else copy(size = size - space).some

    def add(space: Int): Block = copy(size = size + space)

  def take(blocks: List[Block], size: Int): List[Block] =
    def go(sizeLeft: Int, taken: List[Block], blocks: List[Block]): List[Block] =
      if sizeLeft <= 0 then taken
      else
        blocks match
          case b :: t =>
            b.remove(sizeLeft)
              .fold(go(sizeLeft - b.size, taken :+ b, t)): bigBlock =>
                go(
                  sizeLeft - bigBlock.size,
                  taken :+ bigBlock.copy(size = sizeLeft),
                  bigBlock.copy(size = sizeLeft) +: t
                )
          case Nil => taken

    go(size, List.empty, blocks)

  def drop(blocks: List[Block], size: Int): List[Block] =
    def go(sizeLeft: Int, dropped: List[Block]): List[Block] =
      if sizeLeft <= 0 then dropped
      else
        dropped match
          case b :: t =>
            b.remove(sizeLeft)
              .fold(go(sizeLeft - b.size, t)): bigBlock =>
                go(sizeLeft - b.size, bigBlock +: t)
          case Nil => dropped

    go(size, blocks)

  def pack(blocks: List[Block]): List[Block] =
    val (packed, lastOne) = blocks.foldLeft((List.empty[Block], none[Block])):
      case ((packed, None), block) =>
        (packed, block.some)
      case ((packed, Some(current)), block) if current.id == block.id =>
        (packed, current.add(block.size).some)
      case ((packed, Some(current)), block) =>
        (packed :+ current, block.some)

    lastOne.fold(packed)(packed.appended)

  override def parse(input: String): List[Block] = input
    .takeWhile(c => c >= 48 && c <= 57)
    .zipWithIndex
    .toList
    .map:
      case (char, i) if i % 2 == 0 =>
        Block(size = char.toString.toInt, id = i / 2, empty = false)
      case (char, i) =>
        Block(size = char.toString.toInt, id = -999, empty = true)

  def defrag(blocks: List[Block]): List[Block] =
    @tailrec
    def process(defragged: List[Block], filesToDefrag: List[Block], disk: List[Block]): List[Block] =
      println("=====================")
      println(s"defragged: ${render(defragged)}")
      println(s"files left: ${render(filesToDefrag)}, $filesToDefrag")
      println(s"disk: ${render(disk)}")

      filesToDefrag match
        case Nil => defragged
        case _ =>
          disk match
            case e :: rest if e.empty =>
              println(s"adding ${take(filesToDefrag, e.size)} to empty spot")
              process(defragged ++ take(filesToDefrag, e.size), drop(filesToDefrag, e.size), rest)
            case num :: rest =>
              println(s"copying $num to defragged")
              process(defragged :+ num, filesToDefrag, rest)
            case Nil => defragged

    val freeSpace = blocks.filter(_.empty).map(_.size).sum

    println(blocks.reverse)
    println(freeSpace)
    println("!")
    println(take(blocks.reverse, freeSpace).filterNot(_.empty))
    println(
      take(blocks.reverse, freeSpace).filterNot(_.empty) == List(
        Block(2, 9, false),
        Block(4, 8, false),
        Block(3, 7, false),
        Block(3, 6, false)
      )
    )
    println("!")

    process(List.empty, take(blocks.reverse, freeSpace).filterNot(_.empty), blocks)

  def checksum(blocks: List[Block]): BigInt =
    blocks
      .takeWhile(!_.empty)
      .zipWithIndex
      .foldLeft(BigInt(0)):
        case (sum, (char, idx)) =>
          sum + (char.size * idx)

  def render(blocks: List[Block]): String =
    blocks
      .flatMap: block =>
        val ch = if block.empty then "." else block.id
        List.fill(block.size)(ch)
      .mkString

  override def part1(input: List[Block]): BigInt =
    checksum(defrag(input))
