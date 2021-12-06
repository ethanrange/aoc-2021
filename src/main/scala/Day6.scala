import scala.io.Source
import scala.annotation.tailrec

class Day6 {
  val first = Source.fromFile("src/main/data/day6/input.txt").getLines.toSeq(0)
  val digits = first.split(",").map(_.toInt).toSeq

  val cache = collection.mutable.Map.empty[(Int, Int), BigInt]

  def calc(a: Int, b: Int): BigInt = cache.getOrElseUpdate(
    (a, b), if (a >= b) 1 else (calc(6, b - a - 1) + calc(8, b - a - 1)))

  def star1(): BigInt = digits.map(calc(_, 80)).sum

  def star2(): BigInt = digits.map(calc(_, 256)).sum
}
