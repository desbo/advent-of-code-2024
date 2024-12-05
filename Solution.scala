import cats.Show

trait Solution[In, Out](using val SR: Show[Out]) {
  def parse(input: String): In

  def part1(input: In): Out = ???
  def part2(input: In): Out = ???
}
