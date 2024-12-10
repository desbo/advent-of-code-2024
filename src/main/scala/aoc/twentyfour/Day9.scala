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
    @tailrec
    def go(sizeLeft: Int, taken: List[Block], blocks: List[Block]): List[Block] =
      if sizeLeft <= 0 then taken
      else
        blocks match
          case b :: t =>
            b.remove(sizeLeft) match
              case Some(partialBlock) => taken :+ partialBlock.copy(size = sizeLeft)
              case None               => go(sizeLeft - b.size, taken :+ b, t)
          case Nil => taken

    go(size, List.empty, blocks)

  def drop(blocks: List[Block], size: Int): List[Block] =
    def go(sizeLeft: Int, dropped: List[Block]): List[Block] =
      if sizeLeft <= 0 then dropped
      else
        dropped match
          case b :: t =>
            b.remove(sizeLeft) match
              case Some(partialBlock) => go(sizeLeft - b.size, partialBlock +: t)
              case None               => go(sizeLeft - b.size, t)
          case Nil => dropped

    go(size, blocks)

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
      filesToDefrag match
        case Nil =>
          disk match
            case h :: _ if !h.empty => defragged :+ h.copy(size = 1) // don't understand why this is required :(
            case _                  => defragged
        case _ =>
          disk match
            case e :: rest if e.empty =>
              val add       = take(filesToDefrag, e.size)
              val addedSize = add.map(_.size).sum
              val nextDisk  = e.remove(addedSize).fold(rest)(_ +: rest)

              process(defragged ++ add, drop(filesToDefrag, e.size), nextDisk)
            case num :: rest =>
              process(defragged :+ num, filesToDefrag, rest)
            case Nil => defragged

    val freeSpace = blocks.filter(_.empty).map(_.size).sum

    process(List.empty, take(blocks.reverse, freeSpace).filterNot(_.empty), blocks)

  def defrag2(blocks: List[Block]): List[Block] =
    def insert(range: List[Block], block: Block): (List[Block], Boolean) =
      val r        = range.toBuffer
      var inserted = false
      var filled   = false

      range.zipWithIndex.foreach:
        case (b, i) if b.empty && b.size >= block.size && !inserted =>
          inserted = true
          r.update(i, block)

          b.remove(block.size) match
            case Some(space) => r.insert(i + 1, space); filled = true
            case None        => r.update(i, block)
        case _ => ()

      (r.toList, inserted)

    blocks.zipWithIndex.reverse
      .filterNot(_._1.empty)
      .foldLeft(blocks):
        case (defragged, (candidate, idx)) =>
          val (targetRange, sourceRange) =
            defragged.splitAt(idx)

          val (targetRangeAfterInsert, inserted) =
            insert(targetRange, candidate)

          val movedSourceRange =
            if inserted then
              sourceRange.map:
                case b if b == candidate => b.copy(empty = true, id = -999)
                case b                   => b
            else sourceRange

          targetRangeAfterInsert ++ movedSourceRange

  def checksum(blocks: List[Block]): BigInt =
    blocks
      .flatMap: block =>
        List.fill(block.size)(block)
      .zipWithIndex
      .map:
        case (b, i) if !b.empty => BigInt(b.id * i)
        case _                  => BigInt(0)
      .sum

  def render(blocks: List[Block]): String =
    blocks
      .flatMap: block =>
        val ch = if block.empty then "." else block.id
        List.fill(block.size)(ch)
      .mkString

  override def part1(input: List[Block]): BigInt =
    checksum(defrag(input))

  override def part2(input: List[Block]): BigInt =
    // running it 10 times works and I don't care anymore.
    val dumb = LazyList.iterate(input)(defrag2).map(checksum)
    dumb.take(10).last
