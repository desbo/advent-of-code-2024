package aoc
package twentyfour

import Day17.*
import cats.syntax.all.*

import scala.annotation.tailrec

object Day17 extends Solution[Program, String]:
  case class Program(registers: Registers, program: Vector[Int], pointer: Int):
    def op: (Int, Int) = program(pointer) -> program(pointer + 1)
    def operandValue(operand: Int): Long = operand match
      case o if o >= 0 && o <= 3 => o
      case 4                     => registers.a
      case 5                     => registers.b
      case 6                     => registers.c
      case _                     => sys.error(s"invalid operator $operand")

    def setA(value: Long): Program = copy(registers = registers.copy(a = value))
    def setB(value: Long): Program = copy(registers = registers.copy(b = value))
    def setC(value: Long): Program = copy(registers = registers.copy(c = value))

    def setPointer(i: Int): Option[Program] =
      if i >= program.length then none
      else copy(pointer = i).some

    val advance: Option[Program] = setPointer(pointer + 2)

  case class Registers(a: Long, b: Long, c: Long)

  override def parse(input: String): Program =
    input.linesIterator.toList match
      case List(a, b, c, _, prog) =>
        def parseRegister(s: String): Long = s.split(": ").last.toLong
        val r                              = Registers(parseRegister(a), parseRegister(b), parseRegister(c))
        Program(registers = r, program = prog.split(": ").last.split(",").toVector.map(_.toInt), 0)

  def run(program: Option[Program], output: List[Long]): List[Long] =
    program.fold(output): program =>
      import program.*
      def divA(operand: Int): Long = registers.a / math.pow(2, operandValue(operand)).toLong

      op match
        case (0, operand) => run(setA(divA(operand)).advance, output)
        case (1, operand) =>
          val xor = registers.b ^ operand
          run(setB(xor).advance, output)
        case (2, operand) => run(setB(operandValue(operand) % 8).advance, output)
        case (3, operand) =>
          registers.a match
            case 0    => run(advance, output)
            case jump => run(setPointer(operand), output)
        case (4, operand) => run(setB(registers.b ^ registers.c).advance, output)
        case (5, operand) => run(advance, output :+ operandValue(operand) % 8)
        case (6, operand) => run(setB(divA(operand)).advance, output)
        case (7, operand) => run(setC(divA(operand)).advance, output)

  override def part1(input: Program): String =
    run(input.some, List.empty).mkString(",")

  override def part2(input: Program): String =
    def num(output: List[Long]): BigInt = BigInt(output.mkString)
    val target                          = num(input.program.toList.map(_.toLong))

    def binarySearch(min: Long, max: Long): Long =
      if min > max then -1
      else
        val mid    = (min + max) / 2
        val result = num(run(input.setA(mid).some, List.empty))
        println(min -> max -> mid -> result -> target -> (if result > target then "bigger" else "smaller"))
        if result == target then mid
        else if result > target then binarySearch(min, mid - 1)
        else binarySearch(mid + 1, max)

//    run(input.setA(35184372088836L).some, List.empty).mkString(",")

    binarySearch(0, Long.MaxValue).toString
