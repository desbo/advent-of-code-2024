package aoc

import cats.Show

trait Solution[In, Out: Show] {
  type O = Out

  def parse(input: String): In

  def part1(input: In): Out
  def part2(input: In): Out
}

object Solution {


  trait RawInput[Out] extends Solution[String, Out] {
    override def parse(input: String): String = input

    override def part1(input: String): Out
    override def part2(input: String): Out
  }
}
