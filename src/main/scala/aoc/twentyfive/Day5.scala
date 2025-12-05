package aoc.twentyfive

import aoc.Solution
import aoc.twentyfive.Day5.*

object Day5 extends Solution[Database, Long]:
  case class Range(start: Long, end: Long):
    val size                       = end - start + 1
    def contains(x: Long): Boolean = x >= start && x <= end

  case class Database(ranges: List[Range], ids: List[Long])

  def compress(ranges: List[Range]): List[Range] =
    val byStart = ranges.sortBy(_.start)
    byStart
      .drop(1)
      .foldLeft(byStart.take(1)):
        case (rs, r) =>
          val curr = rs.last
          if curr.contains(r.start) then
            val updated = Range(curr.start, math.max(curr.end, r.end))
            rs.dropRight(1).appended(updated)
          else rs.appended(r)

  override def parse(input: String): Database =
    val List(r, i) = input.split("\n\n", 2).toList
    val ranges = r.linesIterator.toList.map: s =>
      val List(a, b) = s.split("-", 2).toList
      Range(a.toLong, b.toLong)
    val ids = i.linesIterator.toList.map(_.toLong)
    Database(ranges, ids)

  override def part1(input: Database): Long =
    input.ids.count: id =>
      input.ranges.exists(_.contains(id))

  override def part2(input: Database): Long =
    compress(input.ranges).foldLeft(0L):
      case (sum, r) => sum + r.size
