package aoc
package twentyfour

import cats.syntax.all.*

import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object Day6 extends Solution[Day6.Lab, Int]:
  case class Point(x: Int, y: Int)

  sealed trait Direction
  object Up    extends Direction
  object Left  extends Direction
  object Right extends Direction
  object Down  extends Direction

  case class Lab(map: List[String], obstacles: Set[Point], guard: Guard):
    def render(obstructions: Set[Point]): String =
      map.zipWithIndex.map: 
        case (s, y) => s.zipWithIndex.map:
          case (c, x) => if obstructions.contains(Point(x,y)) then 'O' else c
        .mkString
      .mkString("\n")

    def addObstruction(p: Point): Lab = copy(obstacles = obstacles + p)
    def obstructed: Boolean           = obstacles.contains(guard.step.position)
    def leaving: Boolean = guard.direction match
      case Up    => guard.position.y < 0
      case Left  => guard.position.x < 0
      case Right => guard.position.x >= map.head.length
      case Down  => guard.position.y >= map.length

  case class Guard(position: Point, direction: Direction, visits: ListSet[Point]):
    def turnRight: Guard = copy(direction = direction match
      case Up    => Right
      case Left  => Up
      case Right => Down
      case Down  => Left
    )

    def step: Guard = copy(
      position = direction match
        case Up    => Point(position.x, position.y - 1)
        case Left  => Point(position.x - 1, position.y)
        case Right => Point(position.x + 1, position.y)
        case Down  => Point(position.x, position.y + 1),
      visits = visits + position
    )

  override def parse(input: String): Lab =
    val (points, guard) = input.linesIterator.zipWithIndex.foldLeft(Set.empty[Point], none[Guard]):
      case (acc, (line, y)) =>
        line.zipWithIndex.foldLeft(acc):
          case ((os, g), (char, x)) =>
            char match
              case '#' => (os + Point(x, y), g)
              case '^' => (os, Guard(Point(x, y), Up, ListSet.empty).some)
              case '<' => (os, Guard(Point(x, y), Left, ListSet.empty).some)
              case '>' => (os, Guard(Point(x, y), Right, ListSet.empty).some)
              case 'v' => (os, Guard(Point(x, y), Down, ListSet.empty).some)
              case _   => (os, g)

    Lab(input.linesIterator.toList, points, guard.get)

  @tailrec
  def patrol(lab: Lab): Lab =
    if lab.leaving then lab
    else
      val nextGuard = if lab.obstructed then lab.guard.turnRight else lab.guard.step
      patrol(lab.copy(guard = nextGuard))

  def cyclic(lab: Lab, visits: Set[(Point, Direction)]): Boolean =
    if lab.leaving then false
    else
      val nextGuard = if lab.obstructed then lab.guard.turnRight else lab.guard.step
      if visits.contains(nextGuard.position -> nextGuard.direction) then true
      else
        cyclic(
          lab.copy(guard = nextGuard),
          visits + (lab.guard.position -> lab.guard.direction)
        )

  override def part1(lab: Lab): Int =
    patrol(lab).guard.visits.size

  override def part2(lab: Lab): Int =
    import cats.effect.*
    import cats.effect.unsafe.implicits.global

    val route = patrol(lab).guard.visits - lab.guard.position

    route.toList
      .traverse: p =>
        IO {
          if cyclic(lab.addObstruction(p), Set.empty) then p.some
          else none
        }.start
      .flatMap(_.traverse(_.join.flatMap(_.fold(none.pure[IO], _ => none.pure[IO], identity))))
      .map(_.flatten.toSet)
      // .flatTap(obs => IO.println(lab.render(obs)))
      .map(_.size)
      .unsafeRunSync()