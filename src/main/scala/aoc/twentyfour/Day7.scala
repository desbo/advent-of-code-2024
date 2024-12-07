package aoc
package twentyfour

import cats.syntax.all.*

import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object Day7 extends Solution[List[Day7.Equation], BigInt]:
  case class Equation(target: BigInt, values: List[Int])

  override def parse(input: String): List[Equation] =
    input.linesIterator.toList.map: s =>
      s.split(": ").toList match
        case List(target, rest) => Equation(BigInt(target), rest.split(" ").map(_.toInt).toList)

  def valid(operators: Set[(BigInt, BigInt) => BigInt])(equation: Equation): Boolean =
    def go(xs: List[Int], x: BigInt): Boolean =
      if x > equation.target then false
      else
        xs match
          case Nil       => x == equation.target
          case a :: rest => operators.exists(o => go(rest, o(x, a)))

    go(equation.values.tail, equation.values.head)

  override def part1(input: List[Equation]): BigInt =
    val ops = Set[(BigInt, BigInt) => BigInt](
      (a, b) => a + b,
      (a, b) => a * b
    )

    input.filter(valid(ops)).map(_.target).sum

  override def part2(input: List[Equation]): BigInt =
    val ops = Set[(BigInt, BigInt) => BigInt](
      (a, b) => a + b,
      (a, b) => a * b,
      (a, b) => BigInt(a.toString.concat(b.toString))
    )

    input.filter(valid(ops)).map(_.target).sum
