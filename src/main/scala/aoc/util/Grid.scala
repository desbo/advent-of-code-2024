package aoc
package util

import cats.Functor

case class Grid[A](data: Vector[Vector[A]]):
  lazy val index: Grid[(A, Vec2)] = Grid[(A, Vec2)]:
    data.zipWithIndex.map:
      case (as, y) =>
        as.zipWithIndex.map:
          case (a, x) =>
            (a, Vec2(x, y))

  lazy val points: Map[Vec2, A] =
    index.data.foldLeft(Map.empty[Vec2, A]):
      case (points, as) =>
        points ++ as.foldLeft(points):
          case (points, (a, point)) => points.updated(point, a)

  def covers(point: Vec2): Boolean =
    points.keySet.contains(point)

object Grid:
  def chars(input: String): Grid[Char] =
    Grid(input.linesIterator.toVector.map(_.toVector))

  given Functor[Grid] with
    override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] =
      Grid(fa.data.map(_.map(f)))
