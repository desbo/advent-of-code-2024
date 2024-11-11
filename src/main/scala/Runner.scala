package aoc

import cats.effect.{IO, IOApp}
import cats.syntax.all.*

import java.time.Duration

object Runner extends IOApp.Simple:
  val solutions = Map(
    1 -> Day1
  )

  override def run: IO[Unit] = solveAllDays

  def solveAllDays: IO[Unit] = (1 to 25)
    .flatMap(day => solutions.get(day).map(day -> _))
    .toList
    .traverse: (day, solution) =>
      runSolution(solution).flatMap: parts =>
        IO.println(s"day $day:") >> parts.traverse:
          case (part, (result, duration)) =>
            IO.println(s"\tpart $part:\n\t\tanswer\t= $result\n\t\truntime\t= ${duration.toNanos / 1000000d}ms")
    .void

  def runSolution[A](solution: Solution[_, A]) =
    val input  = "a"
    val parsed = solution.parse(input)

    def runPart(partNum: Int) =
      for
        start  <- IO.realTimeInstant
        result <- IO(if (partNum == 1) solution.part1(parsed) else solution.part2(parsed))

        end <- IO.realTimeInstant
      yield (result, Duration.between(start, end))

    List(1, 2).traverseFilter: num =>
      runPart(num)
        .map(r => (num, r).some)
        .recover:
          case _: NotImplementedError =>
            none
