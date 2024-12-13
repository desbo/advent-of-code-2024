package aoc
package twentyfour

import util.*
import scala.util.matching.Regex

object Day13 extends Solution[List[Day13.ClawConfig], BigInt]:
  case class ClawConfig(buttonA: BigVec2, buttonB: BigVec2, prize: BigVec2)

  override def parse(input: String): List[ClawConfig] =
    val button = """Button .: X\+(\d+), Y\+(\d+)""".r
    val prize  = """Prize: X=(\d+), Y=(\d+)""".r

    def parse(r: Regex, s: String): BigVec2 =
      s match
        case r(a, b) => BigVec2(a.toInt, b.toInt)

    input
      .split("\n\n")
      .toList
      .map: line =>
        line.split("\n").toList match
          case List(a, b, p) =>
            ClawConfig(parse(button, a), parse(button, b), parse(prize, p))

  def minCost(claw: ClawConfig): BigInt =
    import claw.*

    val d = (buttonA.x * buttonB.y - buttonA.y * buttonB.x)

    val a = (prize.x * buttonB.y - prize.y * buttonB.x) / d
    val b = (buttonA.x * prize.y - buttonA.y * prize.x) / d

    if BigVec2(claw.buttonA.x * a + claw.buttonB.x * b, claw.buttonA.y * a + claw.buttonB.y * b) == prize then a * 3 + b
    else BigInt(0)

  override def part1(input: List[ClawConfig]): BigInt =
    input.map(minCost).sum

  override def part2(input: List[ClawConfig]): BigInt =
    val add = BigInt("10000000000000")

    input
      .map: c =>
        val adjusted = c.copy(prize = BigVec2(c.prize.x + add, c.prize.y + add))
        minCost(adjusted)
      .sum
