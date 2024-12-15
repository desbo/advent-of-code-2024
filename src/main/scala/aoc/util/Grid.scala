package aoc
package util

import cats.{Eval, Foldable, Functor}
import cats.syntax.all.*

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

  lazy val positions: Map[A, Set[Vec2]] =
    points.groupBy(_._2).view.mapValues(_.keySet).toMap

  def update(pos: Vec2, a: A): Grid[A] =
    Grid:
      data.updated(pos.y, data(pos.y).updated(pos.x, a))

  def swap(a: Vec2, b: Vec2): Grid[A] =
    index.map:
      case (_, v) if v == a => points(b)
      case (_, v) if v == b => points(a)
      case (a, _)           => a

  def covers(point: Vec2): Boolean =
    points.keySet.contains(point)

object Grid:
  def chars(input: String): Grid[Char] =
    Grid(input.linesIterator.toVector.map(_.toVector))

  given Functor[Grid] with
    override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] =
      Grid(fa.data.map(_.map(f)))

  given Foldable[Grid] with
    override def foldLeft[A, B](fa: Grid[A], b: B)(f: (B, A) => B): B =
      fa.data.foldl(b)((b, v) => v.foldl(b)(f))

    override def foldRight[A, B](fa: Grid[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.data.foldr(lb)((vec, b) => vec.foldr(b)(f))
