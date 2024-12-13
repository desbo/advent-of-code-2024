package aoc
package util

case class Vec2(x: Int, y: Int):
  def -(other: Vec2): Vec2 =
    Vec2(x - other.x, y - other.y)

  def +(other: Vec2): Vec2 =
    Vec2(x + other.x, y + other.y)

    
case class BigVec2(x: BigInt, y: BigInt):
  def -(other: BigVec2): BigVec2 =
    BigVec2(x - other.x, y - other.y)

  def +(other: BigVec2): BigVec2 =
    BigVec2(x + other.x, y + other.y)
