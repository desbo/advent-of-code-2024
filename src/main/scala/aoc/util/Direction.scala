package aoc.util

import cats.syntax.all.*

sealed trait Direction
object Direction:
  case object Up    extends Direction
  case object Right extends Direction
  case object Left  extends Direction
  case object Down  extends Direction

  def from(c: Char): Option[Direction] = c match
    case '<' => Left.some
    case 'v' => Down.some
    case '>' => Right.some
    case '^' => Up.some
    case _   => none

  val all = List(Up, Right, Down, Left)

  def move(start: Vec2, direction: Direction, distance: Int = 1): Vec2 =
    direction match
      case Up    => Vec2(start.x, start.y - distance)
      case Left  => Vec2(start.x - distance, start.y)
      case Right => Vec2(start.x + distance, start.y)
      case Down  => Vec2(start.x, start.y + distance)

  def neighbours(from: Vec2, directions: List[Direction] = all, distance: Int = 1): List[Vec2] =
    directions.map: d =>
      move(from, d, distance)
