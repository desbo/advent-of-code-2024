package aoc

import cats.Show
import cats.syntax.all.*

import scala.jdk.CollectionConverters.*

trait Solution[In, Out](using val SR: Show[Out]) {
  def parse(input: String): In

  def part1(input: In): Out = ???
  def part2(input: In): Out = ???
}
