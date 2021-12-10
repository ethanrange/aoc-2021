import scala.io.Source

class Day10 {
  val lines = Source
    .fromFile("src/main/data/day10/input.txt")
    .getLines
    .map(_.toCharArray.toSeq)
    .toSeq

  val pairs = Map('<' -> '>', '(' -> ')', '[' -> ']', '{' -> '}')
  val score =
    Map(')' -> (3, 1), ']' -> (57, 2), '}' -> (1197, 3), '>' -> (25137, 4))

  def corrupted(line: Seq[Char]): Either[Char, Seq[Char]] =
    Right(line.foldLeft(Seq(): Seq[Char]) { case (c, n) =>
      if (pairs.contains(n)) (n +: c)
      else if (pairs(c.head) != n) return Left(n)
      else c.tail
    })

  def median(list: Seq[BigInt]) = list.sortWith(_ < _)(list.size / 2)

  def star1(): Int =
    lines.map(corrupted(_)).collect { case Left(c) => score(c)._1 }.sum

  def star2(): BigInt = {
    median(lines.map(corrupted(_)).collect { case Right(l) =>
      l.foldLeft(BigInt(0)) { case (t, c) => t * 5 + score(pairs(c))._2 }})
  }
}
