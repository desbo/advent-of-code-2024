package aoc
package twentyfour

import aoc.util.*
import cats.syntax.all.*

import scala.annotation.tailrec

object Day15 extends Solution[Day15.Input, Long]:
  case class Input(warehouse: Grid[Char], moves: List[Direction]):
    def robot           = warehouse.positions('@').head
    def dropMove: Input = copy(moves = moves.drop(1))

    def render: String = warehouse.data.map(_.mkString).mkString("\n")

    def moveRobot(direction: Direction, boxes: Int = 0): Input =
      val nextPos = robot.offset(direction, boxes + 1)
      warehouse.points.get(nextPos) match
        case Some('.') =>
          copy(warehouse = (boxes to 0 by -1).foldLeft(warehouse):
            case (wh, i) =>
              wh.swap(
                robot.offset(direction, i + 1),
                robot.offset(direction, i)
              )
          )

        case Some('O') => moveRobot(direction, boxes + 1)
        case _         => Input(warehouse, moves)

  override def parse(input: String): Input =
    input.split("\n\n").toList match
      case List(wh, moves) => Input(Grid.chars(wh), moves.toList.flatMap(Direction.from))

  @tailrec
  def run(input: Input): Input =
    input.moves match
      case m :: _ => run(input.moveRobot(m).dropMove)
      case Nil    => input

  override def part1(input: Input): Long =
    run(input).warehouse.index
      .map:
        case ('O', vec) => 100L * vec.y + vec.x
        case _          => 0
      .sumAll
