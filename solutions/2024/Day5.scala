object Day5 extends Solution[Day5.Input, Int]:
  case class Input(rules: Set[Rule], pages: List[List[Int]])
  case class Rule(before: Int, after: Int)

  override def parse(input: String): Input =
    input.linesIterator.foldLeft(Input(Set.empty, List.empty)):
      case (input, line) =>
        line.split('|').toList match
          case List(l, r)              => input.copy(rules = input.rules + Rule(l.toInt, r.toInt))
          case _ if line.contains(",") => input.copy(pages = input.pages :+ line.split(',').map(_.toInt).toList)
          case _                       => input

  def expectedRules(page: List[Int]): Set[Rule] =
    page.zipWithIndex
      .foldLeft(Set.empty[Rule]):
        case (rules, (num, idx)) =>
          page.splitAt(idx) match
            case (Nil, after) =>
              rules ++ after.map(a => Rule(num, a))
            case (before, after) =>
              rules ++ before.tail.map(s => Rule(s, num)).toSet ++ after.map(a => Rule(num, a))
      .filter(r => r.before != r.after)

  def correct(rules: Set[Rule])(page: List[Int]): Boolean =
    expectedRules(page).forall(rules.contains)

  def middle(page: List[Int]): Int =
    page(page.length / 2)

  def sort(rules: Set[Rule])(pages: List[Int]): List[Int] =
    pages.sortWith: (a, b) =>
      rules.contains(Rule(a, b))

  override def part1(input: Input): Int =
    input.pages
      .filter(correct(input.rules))
      .map(middle)
      .sum

  override def part2(input: Input): Int =
    input.pages
      .filterNot(correct(input.rules))
      .map(sort(input.rules).andThen(middle))
      .sum
