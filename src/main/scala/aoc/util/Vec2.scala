package aoc
package util

case class Vec2(x: Int, y: Int):
  def -(other: Vec2): Vec2 =
    Vec2(x - other.x, y - other.y)

  def +(other: Vec2): Vec2 =
    Vec2(x + other.x, y + other.y)

  def *(n: Int): Vec2 =
    Vec2(x * n, y * n)

  def offset(direction: Direction, distance: Int = 1): Vec2 =
    Direction.move(this, direction, distance)

case class BigVec2(x: BigInt, y: BigInt):
  def -(other: BigVec2): BigVec2 =
    BigVec2(x - other.x, y - other.y)

  def +(other: BigVec2): BigVec2 =
    BigVec2(x + other.x, y + other.y)
