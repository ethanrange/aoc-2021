import scala.io.Source

object Main extends App {
  val solution = new Solution()

  solution.star1()
  solution.star2()
}

class Solution() {
  val lines = Source.fromFile("./data/day1/depths.txt").getLines.map { _.toInt }.toSeq

  def star1(): Unit = {
    var count = 0

    for (i <- 1 to lines.size - 1) {
      if (lines(i - 1) < lines(i)) count += 1
    }

    println(count)
  }

  def star2(): Unit = {
    var sums = lines.sliding(3).toList.map(_.sum)
    var count = 0

    for (i <- 1 to sums.size - 1) {
      if (sums(i - 1) < sums(i)) count += 1
    }

    println(count)
  }
}
