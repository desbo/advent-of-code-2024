package aoc
package twentyfour

import cats.syntax.all.*

import scala.collection.immutable.Map

object Day11 extends Solution[List[BigInt], BigInt]:
  override def parse(input: String): List[BigInt] =
    input.split(" ").map(BigInt.apply).toList

  def blink(stones: Map[BigInt, BigInt]): Map[BigInt, BigInt] =
    stones.foldLeft(stones):
      case (next, (n, zeroes)) if n == BigInt(0) =>
        next |+| Map(BigInt(1) -> zeroes, n -> -zeroes)

      case (next, (n, evens)) if n.toString.length % 2 == 0 =>
        val (l, r) = n.toString.splitAt(n.toString.length / 2)
        val split =
          if l != r then Map(BigInt(l) -> evens, BigInt(r) -> evens, n -> -evens)
          else Map(BigInt(l)           -> evens * 2, n     -> -evens)

        next |+| split

      case (next, (n, count)) =>
        next |+| Map((n * 2024) -> count, n -> -count)

  def counts(input: List[BigInt]): Map[BigInt, BigInt] =
    input.groupBy(identity).view.mapValues(l => BigInt(l.size)).toMap

  def blinkN(times: Int, stones: Map[BigInt, BigInt]): Map[BigInt, BigInt] =
    List.iterate(stones, times + 1)(blink).last

  override def part1(input: List[BigInt]): BigInt =
    blinkN(25, counts(input)).values.sum

  override def part2(input: List[BigInt]): BigInt =
    blinkN(75, counts(input)).values.sum
