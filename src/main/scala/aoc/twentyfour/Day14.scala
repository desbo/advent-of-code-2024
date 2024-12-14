package aoc
package twentyfour

import cats.effect.IO
import cats.syntax.all.*
import util.*

object Day14 extends Solution[List[Day14.Robot], Long]:
  val width  = 101
  val height = 103

  case class Robot(position: Vec2, velocity: Vec2)

  override def parse(input: String): List[Robot] =
    val pattern = """p=(\d+),(\d+) v=(-?\d+),(-?\d+)""".r

    input.linesIterator.toList.map:
      case pattern(px, py, vx, vy) =>
        Robot(
          Vec2(px.toInt, py.toInt),
          Vec2(vx.toInt, vy.toInt)
        )

  def quadrants(width: Int, height: Int, robots: List[Robot]): (List[Robot], List[Robot], List[Robot], List[Robot]) =
    val (u, d)   = robots.partition(_.position.y < height / 2)
    val (lu, ru) = u.partition(_.position.x < width / 2)
    val (ld, rd) = d.partition(_.position.x < width / 2)
    (lu, ru, ld, rd)

  def render(robots: List[Robot], width: Int, height: Int): String =
    val byPos = robots.groupBy(_.position)

    (0 until height)
      .map: y =>
        (0 until width)
          .map: x =>
            if x == width / 2 || y == height / 2 then " "
            else byPos.get(Vec2(x, y)).fold(".")(_.size)
          .mkString
      .mkString("\n")

  override def part1(input: List[Robot]): Long =
    def run(robots: List[Robot], width: Int, height: Int, steps: Int) =
      robots
        .map:
          case Robot(p, v) =>
            val adjusted = p + (v * steps)
            val next = Vec2(
              (adjusted.x % width + width)   % width,
              (adjusted.y % height + height) % height
            )

            Robot(next, v)
        .filterNot: r =>
          r.position.x == width / 2 || r.position.y == height / 2

    val robots       = run(input, width, height, 100)
    val (a, b, c, d) = quadrants(width, height, robots)

    a.size * b.size * c.size * d.size

  override def part2(input: List[Robot]): Long =
    import cats.effect.unsafe.implicits.global

    def step(robots: List[Robot], width: Int, height: Int): List[Robot] =
      robots
        .map:
          case Robot(p, v) =>
            val adjusted = p + v
            val next = Vec2(
              (adjusted.x % width + width)   % width,
              (adjusted.y % height + height) % height
            )

            Robot(next, v)

    LazyList
      .iterate(input)(step(_, width, height))
      .zipWithIndex
      .find:
        case (robots, i) =>
          val positions = robots.groupBy(_.position).keySet
          positions.size == robots.size
      .get
      ._2
      .toLong
