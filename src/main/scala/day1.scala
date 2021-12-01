import scala.io.Source

class Day1 {
  val lines = Source
    .fromFile("src/main/data/day1/depths.txt")
    .getLines
    .map { _.toInt }
    .toSeq

  def star1(): Int = {
    lines.zip(lines.tail).filter { case (f, s) => f < s }.size
  }

  def star2(): Int = {
    val sums = lines.sliding(3).toList.map(_.sum)

    sums.zip(sums.tail).filter { case (f, s) => f < s }.size
  }
}
